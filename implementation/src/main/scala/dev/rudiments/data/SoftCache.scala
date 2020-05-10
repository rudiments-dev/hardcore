package dev.rudiments.data

import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.types.HardID.HardID1
import dev.rudiments.hardcore.types.SoftID.SoftID1
import dev.rudiments.hardcore.types._

import scala.collection.parallel

class SoftCache(implicit t: Type) extends Adapter[DataCommand, DataEvent] {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

  override def isDefinedAt(x: DataCommand): Boolean = f.isDefinedAt(x)
  override def apply(cmd: DataCommand): DataEvent = f(cmd)

  private val generator = () => SoftID1(content.size)

  val f: DataSkill = {
    List(
      ReadOnly.find,
      ReadOnly.findAll,

      CRUD.create,
      CRUD.createAuto(generator),
      CRUD.update,
      CRUD.delete,

      Batch.createAll,
      Batch.createAllAuto(generator),
      Batch.replaceAll,
      Batch.deleteAll
    )
  }.fold(ReadOnly.count)(_ orElse _)
}


