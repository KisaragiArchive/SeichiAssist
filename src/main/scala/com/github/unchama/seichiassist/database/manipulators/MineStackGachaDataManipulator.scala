package com.github.unchama.seichiassist.database.manipulators

import cats.effect.IO
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.seichiassist.data.MineStackGachaData
import com.github.unchama.seichiassist.database.DatabaseGateway
import com.github.unchama.seichiassist.util.BukkitSerialization
import org.bukkit.Bukkit
import scalikejdbc._

import java.sql.SQLException
import scala.collection.Seq.iterableFactory
import scala.util.Try

class MineStackGachaDataManipulator(private val gateway: DatabaseGateway) {
  private def handleQueryError2[A, L, R](tryStruct: Try[A],
                                         onSQLException: SQLException => L,
                                         onSuccess: A => R): Either[L, R] = {
    tryStruct.toEither.fold({
      case e: SQLException =>
        println("sqlクエリの実行に失敗しました。以下にエラーを表示します")
        e.printStackTrace()
        Left(onSQLException(e))
      case e => throw e
    }, a => Right(onSuccess(a)))
  }

  // TODO: こいつは1つのItemStackを逐一シリアライズしておけば十分なのでストレージ面での節約が見込まれる。というかそうするべき

  private val tableReference: String = s"${gateway.databaseName}.msgachadata"

  def loadMineStackGachaDataIO: IO[Either[RuntimeException, List[MineStackGachaData]]] = IO {
    handleQueryError2(
      Try {
        DB.readOnly { implicit session =>
          sql"""SELECT * FROM $tableReference"""
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
          sql"""truncate table $tableReference"""
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


          sql"""insert into $tableReference (probability,level,obj_name,itemstack) values (?, ?, ?, ?)"""
            .batch(params: _*)
            .apply()
        }
      },
      e => new RuntimeException("FATAL: マインスタックのセーブに失敗しました", e),
      (a: Unit) => ()
    )
  }
}
