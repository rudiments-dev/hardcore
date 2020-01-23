package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, ID}

import scala.collection.parallel

object ReadOnly {
  case class Find[T <: DTO](key: ID[T]) extends DataCommand[T]
  case class Found[T <: DTO](key: ID[T], value: T) extends DataEvent[T]
  case class NotFound [T <: DTO](key: ID[T]) extends DataErrorEvent[T]
  def find[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
  }

  case class FindAll[T <: DTO]() extends DataCommand[T]
  case class FoundAll [T <: DTO](values: Seq[T]) extends DataEvent[T]
  def findAll[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case FindAll() => FoundAll(content.values.toList)
  }

  case class Count  [T <: DTO]() extends DataCommand[T]
  case class Counted[T <: DTO](total: Long) extends DataEvent[T]
  def count[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case Count() => Counted(content.size)
  }

  //TODO case class Query[T <: DTO](?) extends DataCommand[T]
}
