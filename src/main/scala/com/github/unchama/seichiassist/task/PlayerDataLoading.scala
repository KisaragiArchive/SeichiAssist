package com.github.unchama.seichiassist.task

import cats.effect.{IO, Sync}
import com.github.unchama.seichiassist.data.GridTemplate
import com.github.unchama.seichiassist.data.player._
import com.github.unchama.seichiassist.data.player.settings.BroadcastMutingSettings
import com.github.unchama.seichiassist.minestack.MineStackObj
import com.github.unchama.seichiassist.seichiskill.effect.ActiveSkillEffect.NoEffect
import com.github.unchama.seichiassist.seichiskill.effect.{ActiveSkillNormalEffect, ActiveSkillPremiumEffect, UnlockableActiveSkillEffect}
import com.github.unchama.seichiassist.seichiskill.{ActiveSkill, AssaultSkill, SeichiSkill, SeichiSkillUsageMode}
import com.github.unchama.seichiassist.{MineStackObjectList, SeichiAssist}
import com.github.unchama.util.MillisecondTimer
import org.bukkit.Bukkit
import org.bukkit.ChatColor._

import java.text.{ParseException, SimpleDateFormat}
import java.util.{Calendar, UUID}
import scala.collection.mutable

object PlayerDataLoading {

  /**
   * プレイヤーデータロードを実施する処理(非同期で実行すること)
   *
   * @deprecated Should be inlined.
   * @author unchama
   */
  @Deprecated()
  def loadExistingPlayerData(uuid: UUID, playerName: String): PlayerData = {
    val databaseGateway = SeichiAssist.databaseGateway

    val stringUuid = uuid.toString.toLowerCase()
    val databaseName = SeichiAssist.seichiAssistConfig.getDB

    import scalikejdbc._
    import cats.implicits._

    //noinspection MutatorLikeMethodIsParameterless
    def updateLoginInfoF[F[_] : Sync]: F[Unit] = Sync[F].delay {
      DB.localTx { implicit session =>
        sql"""
              |UPDATE $databaseName.playerdata
              |SET loginflag = TRUE,
              |lastquit = CAST(now() as DATETIME)
              |WHERE uuid = $stringUuid""".stripMargin
          .update()
          .apply()
      }
    }

    def loadMineStackF[F[_] : Sync]: F[mutable.HashMap[MineStackObj, Long]] = Sync[F].delay {
      /*
       * TODO:これはここにあるべきではない
       * 格納可能なアイテムのリストはプラグインインスタンスの中に動的に持たれるべきで、
       * そのリストをラップするオブジェクトに同期された形でこのオブジェクトがもたれるべきであり、
       * ロードされるたびに再計算されるべきではない
       */
      val nameObjectMappings: Map[String, MineStackObj] =
        MineStackObjectList.minestacklist.map(obj => obj.mineStackObjName -> obj).toMap

      val objectAmounts = mutable.HashMap[MineStackObj, Long]()

      val entriesInDB = DB.readOnly { implicit session =>
        sql"""
             |SELECT * FROM $databaseName.minestack
             |WHERE player_uuid = $stringUuid""".stripMargin
          .map(rs => {
            val name = rs.string("object_name")
            val amount = rs.long("amount")

            name -> amount
          })
          .toList()
          .apply()
          .toMap
      }

      nameObjectMappings.keySet.intersect(entriesInDB.keySet).foreach { name =>
        // it is safe because intersect-ed key ensures that name exists in both Map
        val obj = nameObjectMappings(name)
        objectAmounts(obj) = objectAmounts(obj)
      }

      val bukkitLogger = Bukkit.getLogger
      // detect excess key
      (entriesInDB.keySet -- nameObjectMappings.keySet).foreach { name =>
        bukkitLogger
          .warning(s"プレーヤー $playerName のMineStackオブジェクト $name は収納可能リストに見つかりませんでした。")
      }

      objectAmounts
    }

    def fetchGridTemplate[F[_] : Sync]: F[Map[Int, GridTemplate]] = Sync[F].delay {
      DB.readOnly { implicit session =>
        sql"""
             |SELECT id, ahead_length, behind_length, right_length, left_length
             |FROM $databaseName.grid_template
             |WHERE designer_uuid = $stringUuid""".stripMargin
          .map { rs =>
            val id = rs.int("id")
            val ahead = rs.int("ahead_length")
            val behind = rs.int("behind_length")
            val right = rs.int("right_length")
            val left = rs.int("left_length")

            id -> new GridTemplate(ahead, behind, right, left)
          }
          .toList()
          .apply()
          .toMap
      }
    }

    def loadGridTemplateF[F[_] : Sync](playerData: PlayerData): F[Unit] = for {
      map <- fetchGridTemplate
    } yield {
      playerData.templateMap = mutable.HashMap.from(map)
    }

    def loadSkillEffectUnlockStateF[F[_] : Sync]: F[Set[UnlockableActiveSkillEffect]] = Sync[F].delay {
      DB.readOnly { implicit session =>
        sql"""
             |SELECT effect_name
             |FROM $databaseName.unlocked_active_skill_effect
             |WHERE player_uuid = $stringUuid""".stripMargin
          .map { rs =>
            val name = rs.string("effect_name")
            val effect = ActiveSkillNormalEffect.withNameOption(name)
              .orElse(ActiveSkillPremiumEffect.withNameOption(name))

            if (effect.isEmpty) {
              Bukkit.getLogger.warning(s"${stringUuid}所有のスキルエフェクト${name}は未定義です")
            }

            effect
          }
          .toList()
          .apply()
          .flatten
          .toSet
      }
    }
    
    def loadSeichiSkillUnlockStateF[F[_] : Sync]: F[Set[SeichiSkill]] = Sync[F].delay {
      DB.readOnly { implicit session => 
        sql"""
             |SELECT skill_name
             |FROM $databaseName.unlocked_seichi_skill
             |WHERE player_uuid = $stringUuid""".stripMargin
          .map { rs =>
            val skillName = rs.string("skill_name")
            val skill = SeichiSkill.withNameOption(skillName)
            if (skill.isEmpty) {
              Bukkit.getLogger.warning(s"${stringUuid}所有のスキル${skillName}は未定義です")
            }

            skill
          }
          .toList()
          .apply()
          .flatten
          .toSet
      }
    }

    def constructPlayerDataF[F[_] : Sync]: F[PlayerData] = for {
      obtainedEffects <- loadSkillEffectUnlockStateF
      obtainedSkills <- loadSeichiSkillUnlockStateF
    } yield {
      DB.readOnly { implicit session =>
        // すべてのカラムを列挙すると保守性とのトレードオフで分が悪くなるのでこれはこのままにしておいたほうが良さそう
        sql"""
             |SELECT *
             |FROM $databaseName.playerdata
             |WHERE uuid = $stringUuid
             |""".stripMargin
          .map { rs =>
            val playerData = new PlayerData(uuid, playerName)

            playerData.settings.shouldDisplayDeathMessages = rs.boolean("killlogflag")
            playerData.settings.shouldDisplayWorldGuardLogs = rs.boolean("worldguardlogflag")

            playerData.settings.multipleidbreakflag = rs.boolean("multipleidbreakflag")

            playerData.settings.pvpflag = rs.boolean("pvpflag")
            playerData.settings.broadcastMutingSettings = BroadcastMutingSettings.fromBooleanSettings(
              rs.boolean("everymessage"),
              rs.boolean("everysound")
            )
            playerData.settings.nickname = PlayerNickname(
              NicknameStyle.marshal(rs.boolean("displayTypeLv")),
              rs.int("displayTitle1No"),
              rs.int("displayTitle2No"),
              rs.int("displayTitle3No")
            )

            playerData.settings.autoMineStack = rs.boolean("minestackflag")

            playerData.skillEffectState = {
              val selectedEffect =
                UnlockableActiveSkillEffect
                  .withNameOption(rs.string("selected_effect"))
                  .filter(a => obtainedEffects.contains(a))

              PlayerSkillEffectState(obtainedEffects, selectedEffect.getOrElse(NoEffect))
            }
            playerData.skillState.set(
              PlayerSkillState.fromUnsafeConfiguration(
                obtainedSkills,
                SeichiSkillUsageMode.withValue(rs.int("serialized_usage_mode")),
                SeichiSkill.withNameOption(rs.string("selected_active_skill")).flatMap {
                  case a: ActiveSkill => Some(a)
                  case _ => None
                },
                SeichiSkill.withNameOption(rs.string("selected_assault_skill")).flatMap {
                  case a: AssaultSkill => Some(a)
                  case _ => None
                }
              )
            ).unsafeRunSync()

            playerData.unclaimedApologyItems = rs.int("numofsorryforbug")
            playerData.regionCount = rs.int("rgnum")
            playerData.playTick = rs.int("playtick")
            playerData.p_givenvote = rs.int("p_givenvote")
            playerData.effectPoint = rs.int("effectpoint")

            playerData.totalexp = rs.int("totalexp")

            playerData.contentsPresentInSharedInventory = {
              val serializedInventory = rs.string("shareinv")
              serializedInventory != null && serializedInventory != ""
            }

            //実績、二つ名の情報
            playerData.p_vote_forT = rs.int("p_vote")
            playerData.giveachvNo = rs.int("giveachvNo")
            playerData.achievePoint = AchievementPoint(
              rs.int("achvPointMAX"),
              rs.int("achvPointUSE"),
              rs.int("achvChangenum")
            )

            //期間限定ログインイベント専用の累計ログイン日数
            playerData.LimitedLoginCount = rs.int("LimitedLoginCount")

            //連続・通算ログインの情報、およびその更新
            // TODO: Calendar.getInstanceとSimpleDateFormatは殺す
            val cal = Calendar.getInstance()
            val sdf = new SimpleDateFormat("yyyy/MM/dd")
            val lastIn = rs.string("lastcheckdate")
            playerData.lastcheckdate = if (lastIn == null || lastIn == "") {
              sdf.format(cal.getTime)
            } else {
              lastIn
            }
            val chain = rs.int("ChainJoin")
            playerData.loginStatus = playerData.loginStatus.copy(consecutiveLoginDays = if (chain == 0) {
              1
            } else {
              chain
            })
            val total = rs.int("TotalJoin")

            playerData.loginStatus = playerData.loginStatus.copy(totalLoginDay = if (total == 0) {
              1
            } else {
              total
            })

            try {
              val TodayDate = sdf.parse(sdf.format(cal.getTime))
              val LastDate = sdf.parse(playerData.lastcheckdate)
              val TodayLong = TodayDate.getTime
              val LastLong = LastDate.getTime

              val dateDiff = (TodayLong - LastLong) / (1000 * 60 * 60 * 24)
              if (dateDiff >= 1L) {
                val newTotalLoginDay = playerData.loginStatus.totalLoginDay + 1
                val newConsecutiveLoginDays =
                  if (dateDiff == 1L) // 連続しているか？
                    playerData.loginStatus.consecutiveLoginDays + 1
                  else
                    1

                playerData.loginStatus =
                  playerData.loginStatus.copy(totalLoginDay = newTotalLoginDay, consecutiveLoginDays = newConsecutiveLoginDays)
              }
            } catch {
              case e: ParseException => e.printStackTrace()
            }

            playerData.lastcheckdate = sdf.format(cal.getTime)

            playerData.ChainVote = rs.int("chainvote")

            //実績解除フラグのBitSet型への復元処理
            //初回nullエラー回避のための分岐
            try {
              val rawTitleBinary = rs.string("TitleFlags").split(",").reverse.dropWhile(_.isEmpty).reverse

              val decodedBits = rawTitleBinary.map { x: String => java.lang.Long.parseUnsignedLong(x, 16) }
              val bitSet = mutable.BitSet.fromBitMask(decodedBits)
              playerData.TitleFlags = bitSet
            } catch {
              case _: Exception =>
                playerData.TitleFlags = new mutable.BitSet(10000)
                playerData.TitleFlags.addOne(1)
            }

            //マナ妖精
            playerData.usingVotingFairy = rs.boolean("canVotingFairyUse")
            playerData.VotingFairyRecoveryValue = rs.int("VotingFairyRecoveryValue")
            playerData.hasVotingFairyMana = rs.int("hasVotingFairyMana")
            playerData.toggleGiveApple = rs.int("toggleGiveApple")
            playerData.toggleVotingFairy = rs.int("toggleVotingFairy")
            playerData.setVotingFairyTime(rs.string("newVotingFairyTime"))
            playerData.p_apple = rs.long("p_apple")


            playerData.giganticBerserk = GiganticBerserk(
              rs.int("GBlevel"),
              rs.int("GBexp"),
              rs.int("GBstage"),
              rs.boolean("isGBStageUp")
            )
            playerData.anniversary = rs.boolean("anniversary")

            playerData
          }
          .single()
          .apply()
          .get
      }
    }

    //sqlコネクションチェック
    databaseGateway.ensureConnection()

    //同ステートメントだとmysqlの処理がバッティングした時に止まってしまうので別ステートメントを作成する
    val program: IO[PlayerData] = for {
      pd <- constructPlayerDataF[IO]
      _ <- updateLoginInfoF[IO]
      _ <- loadGridTemplateF[IO](pd)
      _ <- loadMineStackF[IO]
    } yield {
      pd
    }

    // TODO this may not be needed
    implicit val logger = SeichiAssist.instance.logger
    MillisecondTimer.timeF(program)(s"$GREEN${playerName}のプレイヤーデータ読込完了")
      .unsafeRunSync()
  }
}