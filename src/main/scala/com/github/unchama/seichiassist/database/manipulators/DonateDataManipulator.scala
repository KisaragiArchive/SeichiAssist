package com.github.unchama.seichiassist.database.manipulators

import cats.effect.IO
import com.github.unchama.seichiassist.seichiskill.effect.ActiveSkillPremiumEffect
import com.github.unchama.util.ActionStatus
import org.bukkit.entity.Player
import scalikejdbc._

import scala.util.Try

class DonateDataManipulator {
  def recordPremiumEffectPurchase(player: Player, effect: ActiveSkillPremiumEffect): IO[ActionStatus] = {
    DatabaseRoutines.handleQueryError3(
      DB.localTx { implicit session =>
        sql"""insert into seichiassist.donatedata
             |(playername, playeruuid, effectname, usepoint, date)
             |values (
             |${player.getName},
             |${player.getUniqueId.toString},
             |${effect.entryName},
             |${effect.usePoint},
             |cast(now() as datetime)
             |)""".stripMargin
          .update()
          .apply()
      },
      _ => ActionStatus.Fail
    )(_ => ActionStatus.Ok)
      .map(_.merge)
  }

  def addDonate(name: String, point: Int): ActionStatus = {
    DatabaseRoutines.handleQueryError3(
      DB.localTx { implicit session =>
        sql"""insert into seichiassist.donatedata (playername,getpoint,date) values
             |(
             |'$name',
             |$point,
             |cast( now() as datetime )
             |)""".stripMargin
          .update()
          .apply()
      },
      _ => ActionStatus.Fail
    )(_ => ActionStatus.Ok)
      .map(_.merge)
      .unsafeRunSync()
  }

  import com.github.unchama.seichiassist.database.manipulators.DonateDataManipulator.{Obtained, PremiumPointTransaction, Used}
  def loadTransactionHistoryFor(player: Player): IO[List[PremiumPointTransaction]] = {
    import cats.implicits._

    DatabaseRoutines.handleQueryError3(
      DB.readOnly { implicit session =>
        // ※プレイヤー名は完全一致探索で十分
        sql"""select * from seichiassist.donatedata where playername = ${player.getName}"""
          .map { rs =>
            val getTotal = rs.int("getpoint")
            val useTotal = rs.int("usepoint")
            val date = rs.string("date")

            Option.when(getTotal > 0) {
              Obtained(getTotal, date)
            } orElse Option.when(useTotal > 0) {
              val effectName = rs.string("effectname")
              val nameOrEffect = ActiveSkillPremiumEffect.withNameOption(effectName).toRight(effectName)
              Used(useTotal, date, nameOrEffect)
            }
          }
          .list()
          .apply()
      },
      throw _
    )(_.sequence.getOrElse(Nil))
      .map((_: Either[Nothing, List[PremiumPointTransaction]]).merge)
  }

  def currentPremiumPointFor(player: Player): IO[Int] = {
    loadTransactionHistoryFor(player).map { history =>
      history.map {
        case Obtained(p, _) => p
        case Used(p, _, _) => -p
      }.sum
    }
  }
}

object DonateDataManipulator {
  sealed trait PremiumPointTransaction
  case class Obtained(amount: Int, date: String) extends PremiumPointTransaction
  case class Used(amount: Int, date: String, forPurchaseOf: Either[String, ActiveSkillPremiumEffect]) extends PremiumPointTransaction
}