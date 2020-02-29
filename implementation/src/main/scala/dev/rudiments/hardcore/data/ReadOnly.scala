package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.ID

import scala.collection.parallel

object ReadOnly {
  case class Find[T](key: ID[T]) extends DataCommand[T]
  case class Found[T](key: ID[T], value: T) extends DataEvent[T]
  case class NotFound [T](key: ID[T]) extends DataErrorEvent[T]
  def find[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
  }

  case class FindAll[T]() extends DataCommand[T]
  case class FoundAll [T](values: Seq[T]) extends DataEvent[T]
  def findAll[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case FindAll() => FoundAll(content.values.toList)
  }

  case class Count  [T]() extends DataCommand[T]
  case class Counted[T](total: Long) extends DataEvent[T]
  def count[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case Count() => Counted(content.size)
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
