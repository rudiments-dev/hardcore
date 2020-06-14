package dev.rudiments.data

import dev.rudiments.data.CRUD.Created
import dev.rudiments.hardcore.{Adapter, Command, Result}
import dev.rudiments.hardcore.flow.BulkMutated
import dev.rudiments.hardcore.types.SoftID.SoftID1
import dev.rudiments.hardcore.types._

import scala.collection.parallel

class SoftCache(implicit t: Type) extends Adapter[DataCommand, DataEvent] {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = {
    f(cmd) match {
      case r@Right(evt: Created) =>
        counter += 1
        r
      case r@Right(evt: BulkMutated) =>
        counter = content.size + 1
        r
      case evt => evt
    }
  }

  private val generator = () => SoftID1(counter)
  private var counter: Long = 1

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


