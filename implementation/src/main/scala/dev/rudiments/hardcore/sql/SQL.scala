package dev.rudiments.hardcore.sql

import dev.rudiments.hardcore.data.soft.{DataErrorEvent, DataEvent, DataSkill}
import dev.rudiments.hardcore.types.{SoftInstance, Type}

trait SQL[Trans <: Transaction] {
  val softType: Type
  def exec(transaction: Trans): DataEvent
}
