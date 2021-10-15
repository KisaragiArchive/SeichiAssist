package com.github.unchama.seichiassist.database.manipulators

import cats.data.EitherT
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.data.GachaPrize
import com.github.unchama.seichiassist.database.DatabaseGateway
import com.github.unchama.seichiassist.util.BukkitSerialization
import org.bukkit.Bukkit
import scalikejdbc._

import scala.collection.Iterable.iterableFactory

class GachaDataManipulator(private val gateway: DatabaseGateway) {
  private val tableReference: String = gateway.databaseName + "." + "gachadata"

  //ガチャデータロード
  def loadGachaData(): Boolean = {
    val a = DatabaseRoutines.handleQueryError3(
      {
        DB.readOnly { implicit session =>
          sql"""select * from $tableReference"""
            .map { rs =>
              val restoredInventory = BukkitSerialization.fromBase64(rs.string("itemstack"))
              val restoredItemStack = restoredInventory.getItem(0)
              new GachaPrize(restoredItemStack, rs.double("probability"))
            }
            .list()
            .apply()
        }
      },
      _ => return false
    )(identity).unsafeRunSync().merge

    SeichiAssist.gachadatalist.clear()
    SeichiAssist.gachadatalist.addAll(a)
    true
  }

  //ガチャデータセーブ
  def saveGachaData(): Boolean = {
    val program = for {
      _ <- EitherT(
        DatabaseRoutines.handleQueryError3(
          DB.localTx { implicit session =>
            sql"""truncate table $tableReference"""
              .update()
              .apply()
          },
          _ => return false
        )(identity)
      )
      _ <- EitherT(
        DatabaseRoutines.handleQueryError3(
          DB.localTx { implicit session =>
            val params = SeichiAssist.gachadatalist.map { data =>
              import scala.util.chaining._

              Seq(
                data.probability,
                Bukkit.getServer.createInventory(null, 9 * 1)
                  .tap(_.setItem(0, data.itemStack))
                  .pipe(BukkitSerialization.toBase64)
              )
            }

            sql"""insert into $tableReference (probability, itemstack) values (?, ?)"""
              .batch(params)
              .apply()
          },
          _ => return false
        )(identity)
      )
    } yield true

    program
      .value
      .unsafeRunSync()
      .merge
  }


}
