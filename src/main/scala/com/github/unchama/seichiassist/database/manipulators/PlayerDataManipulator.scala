package com.github.unchama.seichiassist.database.manipulators

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import com.github.unchama.contextualexecutor.builder.ResponseEffectOrResult
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.data.RankData
import com.github.unchama.seichiassist.data.player.PlayerData
import com.github.unchama.seichiassist.database.DatabaseGateway
import com.github.unchama.seichiassist.database.manipulators.DatabaseRoutines.{handleQueryError2, handleQueryError3}
import com.github.unchama.seichiassist.task.{CoolDownTask, PlayerDataLoading}
import com.github.unchama.seichiassist.util.BukkitSerialization
import com.github.unchama.targetedeffect.TargetedEffect
import com.github.unchama.targetedeffect.commandsender.MessageEffect
import com.github.unchama.util.ActionStatus
import org.bukkit.Bukkit
import org.bukkit.ChatColor._
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.inventory.Inventory
import scalikejdbc.{DB, scalikejdbcSQLInterpolationImplicitDef}

import java.sql.SQLException
import java.text.SimpleDateFormat
import java.util.{Calendar, UUID}
import scala.util.Try

class PlayerDataManipulator {
  // TODO: tableReferenceを埋め込んでいるが、これはscalikejdbcだと文脈を考慮せずにクォートでくくられる。インライン展開して死滅させるべき

  private val plugin = SeichiAssist.instance

  private val tableReference: String = s"seichiassist.playerdata"

  /**
   * 投票特典配布時の処理(p_givenvoteの値の更新もココ)
   */
  def fetchLeftVotingCount(player: Player, playerdata: PlayerData): Int = {
    /*
      TODO: ifCoolDownThenGetは単なる時間差分である。わざわざPlayerDataに依存する必要はない。
        これをぶっ潰したあとにUUIDを受け取れば十分なのでそれで代用するべき。
     */
    ifCoolDownDoneThenGet(player, playerdata) {
      val struuid = playerdata.uuid.toString

      val program = for {
        a <- handleQueryError3(
          DB.readOnly { implicit session =>
            sql"select p_vote,p_givenvote from $tableReference where uuid = $struuid"
              .map { rs =>
                (rs.int("p_vote"), rs.int("p_givenvote"))
              }
              .single()
              .apply()
              .get
          },
          _ => {
            player.sendMessage(RED.toString + "投票特典の受け取りに失敗しました")
            return 0
          })(identity)
        (p_vote, p_givenvote) = a.merge
        rest <- if (p_vote > p_givenvote) {
          val e = handleQueryError3(
            {
              DB.localTx { implicit session =>
                sql"""update $tableReference set p_givenvote = $p_vote where uuid = $struuid"""
                  .update()
                  .apply()
              }
            },
            _ => {
              player.sendMessage(RED.toString + "投票特典の受け取りに失敗しました")
              0
            })(_ => p_vote - p_givenvote)
          e.map(_.merge)
        } else IO {
          player.sendMessage(YELLOW.toString + "投票特典は全て受け取り済みのようです")
          0
        }
      } yield rest

      program.unsafeRunSync()
    }
  }

  /**
   * 最新のnumofsorryforbug値を返してmysqlのnumofsorrybug値を初期化する処理
   */
  def givePlayerBug(player: Player): ResponseEffectOrResult[CommandSender, Int] = {
    val uuid = player.getUniqueId.toString
    val program = for {
      numberToGrant <- EitherT(
        handleQueryError3(
          {
          DB.readOnly { implicit session =>
            sql"select numofsorryforbug from $tableReference where uuid = $uuid"
              .map(rs => rs.int("numofsorryforbug"))
              .single()
              .apply()
              .get
          }
        }, _ => {
          MessageEffect(RED.toString + "ガチャ券の受け取りに失敗しました")
        })(Math.min(_, 64 * 9))
      )
      _ <- EitherT(
        handleQueryError3({
          DB.localTx { implicit session =>
            sql"update $tableReference set numofsorryforbug = numofsorryforbug - $numberToGrant where uuid = $uuid"
          }
        }, _ => {
          MessageEffect(RED.toString + "ガチャ券の受け取りに失敗しました")
        })(_ => ())
      )
    } yield numberToGrant

    program.value.unsafeRunSync()
  }

  @inline private def ifCoolDownDoneThenGet(player: Player, playerdata: PlayerData)(supplier: => Int): Int = {
    //連打による負荷防止の為クールダウン処理
    if (!playerdata.votecooldownflag) {
      player.sendMessage(RED.toString + "しばらく待ってからやり直してください")
      return 0
    }
    new CoolDownTask(player, true, false).runTaskLater(plugin, 1200)

    supplier
  }

  /**
   * 永続化層において投票ポイントをインクリメントする。
   *
   * @param playerName プレーヤー名
   */
  def incrementVotePoint(playerName: String): Unit = {
    DB.localTx { implicit session =>
      sql"update playerdata set p_vote = p_vote + 1 where name = $playerName"
        .update()
        .apply()
    }
  }

  //FIXME: please use UUID instead of player's name whenever possible!!!!!!!!!!!!

  /**
   * 指定されたプレイヤーに運営からのお詫びガチャ券を加算する。
   *
   */
  def addPlayerBug(playerName: String, num: Int): ActionStatus = {
    handleQueryError3(Try {
      DB.localTx { implicit session =>
        sql"update $tableReference set numofsorryforbug = numofsorryforbug + $num where name = '$playerName'"
          .update()
          .apply()
      }
    }, _ => ActionStatus.Fail)(_ => ActionStatus.Ok)
      .map(_.merge)
      .unsafeRunSync()
  }

  def addChainVote(name: String): Boolean = {
    val calendar = Calendar.getInstance()
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd")
    DB.localTx { implicit session =>
      val readLastVote =
        sql"SELECT lastvote FROM playerdata WHERE name = $name"
          .map(_.string("lastvote"))
          .headOption()
          .apply()
          .getOrElse(return false)
      val lastVote = if (readLastVote == null || readLastVote == "")
        dateFormat.format(calendar.getTime)
      else
        readLastVote

      sql"UPDATE playerdata SET lastvote = ${dateFormat.format(calendar.getTime)} WHERE name = $name"
        .update().apply()

      val TodayDate = dateFormat.parse(dateFormat.format(calendar.getTime))
      val LastDate = dateFormat.parse(lastVote)
      val TodayLong = TodayDate.getTime
      val LastLong = LastDate.getTime

      val dateDiff = (TodayLong - LastLong) / (1000 * 60 * 60 * 24)
      val shouldIncrementChainVote = dateDiff <= 2L

      val newCount = if (shouldIncrementChainVote) {
        sql"""select chainvote from playerdata where name = $name"""
          .map(_.int("chainvote"))
          .first().apply()
          .get + 1
      } else 1

      sql"""update playerdata set chainvote = $newCount where name = $name"""
        .update()
        .apply()
      true
    }
  }

  /**
   * 全プレイヤーのanniversaryフラグを変更する。
   * @param anniversary 変更する値
   * @return 変更が成功したならtrue、変更に失敗したならfalse
   */
  def setAnniversaryGlobally(anniversary: Boolean): Boolean = {
    handleQueryError3(Try {
      DB.localTx { implicit session =>
        sql"""UPDATE $tableReference SET anniversary = $anniversary""".update().apply()
      }
    }, _ => false)(_ => true).map(_.merge).unsafeRunSync()
  }

  def saveSharedInventory(player: Player, serializedInventory: String): IO[ResponseEffectOrResult[Player, Unit]] = {
    val assertSharedInventoryBeEmpty: EitherT[IO, TargetedEffect[CommandSender], Unit] =
      for {
        sharedInventorySerialized <- EitherT(loadShareInv(player))
        _ <- EitherT.fromEither[IO] {
          if (sharedInventorySerialized != null && sharedInventorySerialized != "")
            Left(MessageEffect(s"${RED}既にアイテムが収納されています"))
          else
            Right(())
        }
      } yield ()

    val writeInventoryData = handleQueryError3(Try {
      DB.localTx { implicit session =>
        sql"""
             |UPDATE $tableReference SET shareinv = $serializedInventory WHERE uuid = ${player.getUniqueId}
        """.stripMargin
          .update()
          .apply()
      }
    }, _ => MessageEffect(s"${RED}アイテムの格納に失敗しました"))(_ => ())

    for {
      _ <- EitherT(checkInventoryOperationCoolDown(player))
      _ <- assertSharedInventoryBeEmpty
      _ <- EitherT(writeInventoryData)
    } yield ()
  }.value

  def loadShareInv(player: Player): IO[ResponseEffectOrResult[CommandSender, String]] = {
    val loadInventoryData: IO[Either[Nothing, String]] = EitherT.right(IO {
      DB.readOnly { implicit session =>
        sql"""SELECT shareinv FROM $tableReference WHERE uuid = ${player.getUniqueId}"""
          .map(rs => rs.string("shareinv"))
          .single()
          .apply()
          .get
      }
    }).value

    for {
      _ <- EitherT(checkInventoryOperationCoolDown(player))
      serializedInventory <- EitherT(catchingDatabaseErrors(player.getName, loadInventoryData))
    } yield serializedInventory
  }.value

  private def catchingDatabaseErrors[R](targetName: String,
                                        program: IO[Either[TargetedEffect[CommandSender], R]]): IO[Either[TargetedEffect[CommandSender], R]] = {
    program.attempt.flatMap {
      case Left(error) => IO {
        Bukkit.getLogger.warning(s"database failure for $targetName.")
        error.printStackTrace()

        Left(MessageEffect(s"${RED}データベースアクセスに失敗しました。"))
      }
      case Right(result) => IO.pure(result)
    }
  }

  private def checkInventoryOperationCoolDown(player: Player): IO[Either[TargetedEffect[CommandSender], Unit]] = {
    val playerData = SeichiAssist.playermap(player.getUniqueId)
    IO {
      //連打による負荷防止
      if (!playerData.shareinvcooldownflag)
        Left(MessageEffect(s"${RED}しばらく待ってからやり直してください"))
      else {
        new CoolDownTask(player, CoolDownTask.SHAREINV).runTaskLater(plugin, 200)
        Right(())
      }
    }
  }

  def clearSharedInventory(uuid: UUID): IO[ResponseEffectOrResult[CommandSender, Unit]] = {
    handleQueryError3(Try {
      DB.localTx { implicit session =>
        sql"""UPDATE $tableReference SET shareinv = '' WHERE uuid = $uuid"""
          .update()
          .apply()
      }
    }, _ => MessageEffect(s"${RED}アイテムのクリアに失敗しました"))(_ => ())
  }

  def selectLeaversUUIDs(days: Int): IO[Either[SQLException, List[UUID]]] = {
    // FIXME: そもそも不適切な入力が認められるならば例外を握りつぶさずに上へ持っていくべき。
    handleQueryError3({
      DB.readOnly { implicit session =>
        sql"""
             |select name, uuid
             |from $tableReference
             |where (
               |(lastquit <= date_sub(curdate(), interval $days day))
               |or (lastquit is null)
             |)
             |and (name != '')
             |and (uuid != '')""".stripMargin.map { rs =>
          // TODO: 前のコミットではUUIDの構築に失敗したときIllegalArgumentExceptionの説明のために
          //       nameカラムをセレクトしていたが、これは本当に必要なのだろうか？
          rs.string("uuid")
        }
          .list()
          .apply()
      }
    }, identity)(list => list.map(UUID.fromString))
  }

  /**
   * 全ランキングリストの更新処理
   *
   * @return 成否…true: 成功、false: 失敗
   *         TODO この処理はDB上と通信を行う為非同期にすべき
   */
  def successRankingUpdate(): Boolean = {
    if (!successPlayTickRankingUpdate()) return false
    if (!successVoteRankingUpdate()) return false
    successAppleNumberRankingUpdate()
  }

  //ランキング表示用にプレイ時間のカラムだけ全員分引っ張る
  private def successPlayTickRankingUpdate(): Boolean = {
    val ranklist = handleQueryError3(
      {
        DB.readOnly { implicit session =>
          sql"""select name,playtick from $tableReference order by playtick desc"""
            .map(rs => {
              import scala.util.chaining._
              new RankData()
                .tap(_.name = rs.string("name"))
                .tap(_.playtick = rs.int("playtick"))
            })
            .toList()
            .apply()
        }
      },
      _ => return false)(identity).unsafeRunSync()

    // TODO: 初期化時に一度だけ生成してオンラインのときはin-placeで更新するべきでは？
    SeichiAssist.ranklist_playtick.clear()
    SeichiAssist.ranklist_playtick.addAll(ranklist.merge)
    true
  }

  //ランキング表示用に投票数のカラムだけ全員分引っ張る
  private def successVoteRankingUpdate(): Boolean = {
    val ranklist = handleQueryError3(
      {
        DB.readOnly { implicit session =>
          sql"""select name,p_vote from $tableReference order by p_vote desc"""
            .map(rs => {
              import scala.util.chaining._
              new RankData()
                .tap(_.name = rs.string("name"))
                .tap(_.playtick = rs.int("p_vote"))
            })
            .list()
            .apply()
        }
      },
      _ => return false)(identity
    ).unsafeRunSync()

    SeichiAssist.ranklist_p_vote.clear()
    SeichiAssist.ranklist_p_vote.addAll(ranklist.merge)
    true
  }

  //ランキング表示用に上げたりんご数のカラムだけ全員分引っ張る
  private def successAppleNumberRankingUpdate(): Boolean = {
    val program = for {
      list <- handleQueryError3({
        DB.readOnly { implicit session =>
          sql"select name,p_apple from $tableReference order by p_apple desc"
            .map(rs => {
              import scala.util.chaining._

              new RankData()
                .tap(_.name = rs.string("name"))
                .tap(_.p_apple = rs.int("p_apple"))
            })
            .list()
            .apply()
        }
      }, _ => return false)(identity)
      sum <- handleQueryError3({
        DB.readOnly { implicit session =>
          sql"""SELECT SUM(p_apple) as sum FROM $tableReference"""
            .map(rs => rs.long("sum"))
            .single()
            .apply()
            .get
        }
      }, _ => return false)(identity)
    } yield {
      SeichiAssist.ranklist_p_apple.clear()
      SeichiAssist.ranklist_p_apple.addAll(list.merge)
      SeichiAssist.allplayergiveapplelong = sum.merge
    }

    program.unsafeRunSync()
    true
  }

  //全員に詫びガチャの配布
  // TODO: use Either
  def addAllPlayerBug(amount: Int): ActionStatus = {
    handleQueryError3({
      DB.localTx { implicit session =>
        sql"update $tableReference set numofsorryforbug = numofsorryforbug + $amount"
          .update()
          .apply()
      }
    }, _ => ActionStatus.Fail)(_ => ActionStatus.Ok).map(_.merge).unsafeRunSync()
  }

  // TODO: BC breaksメモ: DBアクセスに失敗した場合はそれを引きずらずに例外を投げるようにした。
  def selectPocketInventoryOf(uuid: UUID): IO[ResponseEffectOrResult[CommandSender, Inventory]] = {
    handleQueryError3({
      DB.readOnly { implicit session =>
        sql"select inventory from $tableReference where uuid = $uuid"
          .map(rs => BukkitSerialization.fromBase64(rs.string("inventory")))
          .single()
          .apply()
      }
        .get
    }, a => throw new AssertionError("It must not be happen", a))(identity)
  }

  def inquireLastQuitOf(playerName: String): IO[TargetedEffect[CommandSender]] = {
    val fetchLastQuitData = handleQueryError3({
      DB.readOnly { implicit session =>
        sql"select lastquit from $tableReference where name = $playerName"
          .map(rs => rs.string("lastquit"))
          .single()
          .apply()
      }
        .get
    }, a => throw new AssertionError("It must not be happen", a))(identity)

    catchingDatabaseErrors(playerName, fetchLastQuitData).map {
      case Left(errorEffect) =>
        import com.github.unchama.generic.syntax._

        val messages = List(
          s"${RED}最終ログアウト日時の照会に失敗しました。",
          s"${RED}プレイヤー名やプレイヤー名が変更されていないか確認してください。",
          s"${RED}プレイヤー名が正しいのにこのエラーが出る場合、最終ログイン時間が古い可能性があります。"
        )

        errorEffect.followedBy(MessageEffect(messages))
      case Right(lastQuit) =>
        MessageEffect(s"${playerName}の最終ログアウト日時：$lastQuit")
    }
  }

  def loadPlayerData(playerUUID: UUID, playerName: String): PlayerData = {
    val db = SeichiAssist.seichiAssistConfig.getDB

    // TODO: これは外部キーを設定する際の妨げになる。計算コストの無駄なのでやめるべき。
    val stringUuid = playerUUID.toString.toLowerCase()

    //uuidがsqlデータ内に存在するか検索
    val count = DB.readOnly { implicit session =>
      sql"SELECT COUNT(*) as count FROM $db.playerdata WHERE uuid = $stringUuid"
        .map(rs => rs.int("count"))
        .single()
        .apply()
    }.get

    count match {
      case 0 =>
        SeichiAssist.instance.getLogger.info(s"$YELLOW${playerName}は完全初見です。プレイヤーデータを作成します")

        DB.localTx { implicit session =>
          sql"insert into $db.playerdata (name,uuid,loginflag) values($playerName, $stringUuid, '1')"
            .update()
            .apply()
        }

        new PlayerData(playerUUID, playerName)
      case 1 =>
        PlayerDataLoading.loadExistingPlayerData(playerUUID, playerName)

      // カウントは非負整数。かつ、異なる2人以上のプレイヤーが同じUUIDを持っていることは (論理上) 考えられないので、0と1以外の
      // ケースを考慮する必要がない。
    }
  }
}
