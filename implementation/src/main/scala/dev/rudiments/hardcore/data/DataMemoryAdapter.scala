package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types._
import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.types.HardID.HardID1

import scala.collection.parallel

class DataMemoryAdapter[T] extends Adapter[DataCommand[T], DataEvent[T]] {
  private implicit val content: parallel.mutable.ParMap[HardID[T], T] = parallel.mutable.ParMap.empty[HardID[T], T]

  override def isDefinedAt(x: DataCommand[T]): Boolean = f.isDefinedAt(x)
  override def apply(cmd: DataCommand[T]): DataEvent[T] = f(cmd)

  private val generator = () => HardID1[T, Long](content.size)

  val f: DataSkill[T] = {
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


