package dev.rudiments.hardcore.sql.realization.scalalike

import dev.rudiments.hardcore.sql.Transaction
import scalikejdbc.{DBConnection, Tx}

abstract class ScalaLikeTransaction(val dbCon: DBConnection) extends Transaction {
  def underlying: Tx
}
