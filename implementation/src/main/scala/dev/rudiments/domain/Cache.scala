package dev.rudiments.domain

import dev.rudiments.data.{Batch, CRUD, DataEvent, ReadOnly}
import dev.rudiments.hardcore.{Command, Message, Skill}

import scala.collection.parallel

class Cache extends Skill {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Message = {
    f(cmd)
  }

  val f: Skill = {
    Seq(
      ReadOnly.find,
      ReadOnly.findAll,

      CRUD.create,
      CRUD.update,
      CRUD.delete,

      Batch.createAll,
      Batch.replaceAll,
      Batch.deleteAll
    )
  }.fold(ReadOnly.count)(_ orElse _)
}


