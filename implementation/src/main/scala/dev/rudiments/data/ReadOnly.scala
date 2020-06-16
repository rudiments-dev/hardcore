package dev.rudiments.data

import dev.rudiments.hardcore.flow.BulkRead
import dev.rudiments.hardcore.http.query.{PredicatesQuery, Query}
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import cats.Always
import dev.rudiments.hardcore.flow.{AlwaysDo, BulkRead, ReadSingle}
import dev.rudiments.hardcore.types.{ID, Instance}

import scala.collection.parallel

object ReadOnly {
  case class Find     (key: ID)                   extends DataCommand with ReadSingle
  case class Found    (key: ID, value: Instance)  extends DataEvent
  case class NotFound (key: ID)                   extends DataErrorEvent

  def find(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
  }


  case class FindAll(query: Query) extends DataCommand  with BulkRead
  case class FoundAll(values: Seq[Instance]) extends DataEvent

  def findAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(content.values.toList))
  }

  case object Count   extends DataCommand with AlwaysDo
  case class  Counted(total: Long) extends DataEvent

  def count(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Count => Counted(content.size)
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
