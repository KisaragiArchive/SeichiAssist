package com.github.unchama.seichiassist.database.manipulators

import cats.effect.IO

import java.sql.SQLException
import scala.util.Try

object DatabaseRoutines {
  @deprecated("please use handleQueryError3, most of type-inference issues have been resolved on that.")
  private[manipulators] def handleQueryError2[A, L, R](tryStruct: Try[A],
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

  private[manipulators] def handleQueryError3[A, L, R](
                                                        lazySupplier: => A,
                                                        onSQLException: SQLException => L,
                                                        currentAction: String = "SQLクエリの実行"
                                                      )
                                                      (onSuccess: A => R): IO[Either[L, R]] = IO {
    Try(lazySupplier).toEither.fold({
      case e: SQLException =>
        println(s"${currentAction}の実行に失敗しました。以下にエラーを表示します")
        e.printStackTrace()
        Left(onSQLException(e))
      case e => throw e
    }, a => Right(onSuccess(a)))
  }
}
