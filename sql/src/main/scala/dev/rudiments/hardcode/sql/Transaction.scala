package dev.rudiments.hardcode.sql

trait Transaction {

  def commit()
  def rollback()

}

trait TransactionProvider[T <: Transaction] {

  def transaction(): T
}