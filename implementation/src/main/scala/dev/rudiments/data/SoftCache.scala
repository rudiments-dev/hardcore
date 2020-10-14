package dev.rudiments.data

import dev.rudiments.hardcore.{Adapter, Command, Message, Skill}
import dev.rudiments.domain.{ID, Instance, Spec}

import scala.collection.parallel

class SoftCache(implicit spec: Spec) extends Adapter[DataCommand, DataEvent] {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Message = f.apply(cmd)

  val f: Skill = {
    List(
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


