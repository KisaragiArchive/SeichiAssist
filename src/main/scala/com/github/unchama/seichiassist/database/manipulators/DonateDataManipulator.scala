package com.github.unchama.seichiassist.database.manipulators

import cats.effect.IO
import com.github.unchama.generic.effect.SyncExtra
import com.github.unchama.seichiassist.database.manipulators.DonateDataManipulator.{Obtained, PremiumPointTransaction, Used}
import com.github.unchama.seichiassist.database.DatabaseGateway
import com.github.unchama.seichiassist.seichiskill.effect.ActiveSkillPremiumEffect
import com.github.unchama.util.ActionStatus
import org.bukkit.entity.Player
import scalikejdbc._

import scala.util.Try

class DonateDataManipulator(private val gateway: DatabaseGateway) {
  private def tableReference: String = s"seichiassist.donatedata"

  def recordPremiumEffectPurchase(player: Player, effect: ActiveSkillPremiumEffect): IO[ActionStatus] = IO {

    DatabaseRoutines.handleQueryError2(Try {
      DB.localTx { implicit session =>
        sql"""insert into $tableReference
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
      }
    }, _ => ActionStatus.Fail, (_: Unit) => ActionStatus.Ok).merge
  }

  def addDonate(name: String, point: Int): ActionStatus = {
    DatabaseRoutines.handleQueryError2(Try {
      DB.localTx { implicit session =>
        sql"""insert into $tableReference (playername,getpoint,date) values
             |(
             |'$name',
             |$point,
             |cast( now() as datetime )
             |)""".stripMargin
          .update()
          .apply()
      }
    }, _ => ActionStatus.Fail, (_: Unit) => ActionStatus.Ok).merge
  }

  def loadTransactionHistoryFor(player: Player): IO[List[PremiumPointTransaction]] = IO {
    import cats.implicits._

    DatabaseRoutines.handleQueryError2(
      Try {
        DB.readOnly { implicit session =>
          // ※プレイヤー名は完全一致探索で十分
          sql"""select * from $tableReference where playername = ${player.getName}"""
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
        }
      },
      throw _,
      (a: List[Option[PremiumPointTransaction]]) => a.sequence.getOrElse(Nil)
    ).merge
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