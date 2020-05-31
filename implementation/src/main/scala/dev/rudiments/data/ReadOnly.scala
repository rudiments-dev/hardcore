package dev.rudiments.data

import dev.rudiments.hardcore.http.query.HttpQuery
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.types.{ID, Instance}

import scala.collection.parallel

object ReadOnly {
  case class Find     (key: ID)                   extends DataCommand
  case class Found    (key: ID, value: Instance)  extends DataEvent
  case class NotFound (key: ID)                   extends DataErrorEvent

  def find(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
  }


  case class FindAll(query: HttpQuery) extends DataCommand
  case class  FoundAll(values: Seq[Instance]) extends DataEvent

  def findAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(content.values.toList))
  }

  case object Count   extends DataCommand
  case class  Counted(total: Long) extends DataEvent

  def count(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Count => Counted(content.size)
  }

  //TODO case class Query[T](?) extends DataCommand[T]
}
