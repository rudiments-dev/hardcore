package dev.rudiments.hardcode.sql.scalalike

import dev.rudiments.hardcode.sql.TransactionProvider
import scalikejdbc.{DB, Tx}

class ScalaLikeTransactionProvider(db: DB) extends TransactionProvider[ScalaLikeTransaction] {

  override def transaction(): ScalaLikeTransaction = {
//    db.tx.rollbackIfActive()
    val tx = db.newTx
    tx.begin()

    new ScalaLikeTransaction(db) {
      override def commit(): Unit = tx.commit()

      override def rollback(): Unit = tx.rollback()

      override def underlying: Tx = tx
    }
  }

}
