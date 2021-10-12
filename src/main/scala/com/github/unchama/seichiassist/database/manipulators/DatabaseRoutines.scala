package com.github.unchama.seichiassist.database.manipulators

import java.sql.SQLException
import scala.util.Try

object DatabaseRoutines {
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
}
