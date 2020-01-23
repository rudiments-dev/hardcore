package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types._
import dev.rudiments.hardcore.Adapter

import scala.collection.parallel

class DataMemoryAdapter[T <: DTO : HardType] extends Adapter[DataCommand[T], DataEvent[T]] {
  private implicit val content: parallel.mutable.ParMap[ID[T], T] = parallel.mutable.ParMap.empty[ID[T], T]

  override def isDefinedAt(x: DataCommand[T]): Boolean = f.isDefinedAt(x)
  override def apply(cmd: DataCommand[T]): DataEvent[T] = f(cmd)

  val f: DataSkill[T] = {
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


