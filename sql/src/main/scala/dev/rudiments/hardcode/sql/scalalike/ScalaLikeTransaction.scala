package dev.rudiments.hardcode.sql.scalalike

import dev.rudiments.hardcode.sql.Transaction
import scalikejdbc.{DBConnection, Tx}

abstract class ScalaLikeTransaction(val dbCon: DBConnection) extends Transaction {
  def underlying: Tx
}
