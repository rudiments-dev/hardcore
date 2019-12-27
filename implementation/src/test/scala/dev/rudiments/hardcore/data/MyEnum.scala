package dev.rudiments.hardcore.data

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed trait MyEnum extends EnumEntry
object MyEnum extends Enum[MyEnum] {
  override def values: immutable.IndexedSeq[MyEnum] = findValues

  case object One extends MyEnum
  case object Two extends MyEnum
  case object Red extends MyEnum
}