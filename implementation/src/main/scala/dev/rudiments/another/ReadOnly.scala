package dev.rudiments.another

import dev.rudiments.data.{DataCommand, DataErrorEvent, DataEvent, DataSkill}
import dev.rudiments.hardcore.flow.{AlwaysDo, BulkRead}

import scala.collection.parallel

object ReadOnly {
  case class Find     (key: ID)                   extends DataCommand
  case class Found    (key: ID, value: Instance)  extends DataEvent
  case class NotFound (key: ID)                   extends DataErrorEvent

  def find(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value).toEither
        case None => NotFound(key).toEither
      }
  }


  case class FindAll() extends DataCommand  with BulkRead
  case class FoundAll(values: Seq[Instance]) extends DataEvent

  def findAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case FindAll() => FoundAll(content.values.toList).toEither
  }

  case class Count() extends DataCommand with AlwaysDo
  case class Counted(total: Long) extends DataEvent

  def count(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Count() => Counted(content.size).toEither
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
