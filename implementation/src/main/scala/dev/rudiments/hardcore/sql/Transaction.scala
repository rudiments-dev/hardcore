package dev.rudiments.hardcore.sql

trait Transaction {

  def commit()
  def rollback()
//  def actions()
}

trait TransactionProvider[T <: Transaction] {

  def transaction(): T
}