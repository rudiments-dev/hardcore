package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types._
import dev.rudiments.hardcore.{Adapter, Command, Result}
import dev.rudiments.hardcore.types.HardID.HardID1

import scala.collection.parallel

class HardCache[T] extends Adapter[DataCommand[T], DataEvent[T]] {
  private implicit val content: parallel.mutable.ParMap[HardID[T], T] = parallel.mutable.ParMap.empty[HardID[T], T]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent[T]] = f(cmd)

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


