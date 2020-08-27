package dev.rudiments.another

import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcore.{Adapter, Command, Result}

import scala.collection.parallel

class Cache extends Adapter[DataCommand, DataEvent] {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

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


