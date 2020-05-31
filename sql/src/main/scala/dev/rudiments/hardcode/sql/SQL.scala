package dev.rudiments.hardcode.sql

import dev.rudiments.data.DataEvent
import dev.rudiments.hardcore.types.Type

trait SQL[Trans <: Transaction] {
  val softType: Type
  def exec(transaction: Trans): DataEvent
}
