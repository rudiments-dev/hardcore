package dev.rudiments.data

import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.flow.{AlwaysDo, BulkRead, ReadSingle}
import dev.rudiments.domain.{ID, Instance}

import scala.collection.concurrent

object ReadOnly {
  case class Find     (key: ID)                   extends DataCommand with ReadSingle
  case class Found    (key: ID, value: Instance)  extends DataEvent
  case class NotFound (key: ID)                   extends DataErrorEvent

  def find(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value).toEither
        case None => NotFound(key).toEither
      }
  }


  case class FindAll(query: Query) extends DataCommand  with BulkRead
  case class FoundAll(values: Seq[Instance]) extends DataEvent

  def findAll(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(content.values.toList)).toEither
  }

  case class Count() extends DataCommand with AlwaysDo
  case class Counted(total: Long) extends DataEvent

  def count(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case Count() => Counted(content.size).toEither
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
