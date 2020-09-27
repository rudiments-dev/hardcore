package dev.rudiments.data

import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore.Skill

import scala.collection.parallel

object ReadOnly {
  case class Find     (key: ID)                   extends DataCommand
  case class Found    (key: ID, value: Instance)  extends DataEvent
  case class NotFound (key: ID)                   extends DataError

  def find(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
  }


  case class FindAll(query: Query) extends DataCommand
  case class FoundAll(values: Seq[Instance]) extends DataEvent

  def findAll(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
    case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(content.values.toList))
  }

  case class Count() extends DataCommand
  case class Counted(total: Long) extends DataEvent

  def count(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
    case Count() => Counted(content.size)
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
