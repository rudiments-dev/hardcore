package dev.rudiments.domain

import dev.rudiments.data.{Batch, CRUD, DataCommand, DataEvent, DataSkill, ReadOnly}
import dev.rudiments.hardcore.{Adapter, Command, Result}

import scala.collection.concurrent

class Cache extends Adapter[DataCommand, DataEvent] {
  private implicit val content: concurrent.Map[ID, Instance] = concurrent.TrieMap.empty[ID, Instance]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = {
    f(cmd)
  }

  val f: DataSkill = {
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


