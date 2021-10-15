package com.github.unchama.seichiassist.database.manipulators

import cats.effect.IO
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.data.MineStackGachaData
import com.github.unchama.seichiassist.util.BukkitSerialization
import org.bukkit.Bukkit
import scalikejdbc._

import scala.collection.Seq.iterableFactory
import scala.util.Try

class MineStackGachaDataManipulator {

  // TODO: こいつは1つのItemStackを逐一シリアライズしておけば十分なのでストレージ面での節約が見込まれる。というかそうするべき

  def loadMineStackGachaDataIO: IO[Either[RuntimeException, List[MineStackGachaData]]] = IO {
    DatabaseRoutines.handleQueryError2(
      Try {
        DB.readOnly { implicit session =>
          sql"""SELECT * FROM seichiassist.msgachadata"""
            .map { rs =>
              val savedInventory = BukkitSerialization.fromBase64(rs.string("itemstack"))
              val itemStack = savedInventory.getItem(0)

              new MineStackGachaData(
                rs.string("obj_name"),
                itemStack,
                rs.double("probability"),
                rs.int("level")
              )
            }
        }
      },
      e => new RuntimeException("FATAL: MineStack用ガチャデータのロードに失敗しました。", e),
      identity[List[MineStackGachaData]]
    )
  }

  //MineStack用ガチャデータセーブ
  @deprecated("please use saveMineStackGachaDataIO")
  def saveMineStackGachaData(): Boolean = {
    saveMineStackGachaDataIO.unsafeRunSync().isRight
  }

  def saveMineStackGachaDataIO: IO[Either[RuntimeException, Unit]] = IO {
    handleQueryError2(
      Try {
        DB.localTx { implicit session =>
          sql"""truncate table seichiassist.msgachadata"""
            .update()
            .apply()

          import scala.util.chaining._

          val params = SeichiAssist.msgachadatalist.toSeq.map(item =>
            Seq(
              item.probability,
              item.level,
              item.objName,
              BukkitSerialization.toBase64(
                Bukkit.getServer.createInventory(null, 9 * 1)
                  .tap(inv => inv.setItem(0, item.itemStack))
              )
            )
          )


          sql"""insert into seichiassist.msgachadata (probability,level,obj_name,itemstack) values (?, ?, ?, ?)"""
            .batch(params: _*)
            .apply()
        }
      },
      e => new RuntimeException("FATAL: マインスタックのセーブに失敗しました", e),
      (a: Unit) => ()
    )
  }
}
