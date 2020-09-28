package dev.rudiments.data

import dev.rudiments.data.CRUD.Created
import dev.rudiments.hardcore.{Adapter, Command, Result, Success}
import dev.rudiments.domain.{ID, Instance, Spec}

import scala.collection.parallel

class SoftCache(implicit spec: Spec) extends Adapter[DataCommand, DataEvent] {
  private implicit val content: parallel.mutable.ParMap[ID, Instance] = parallel.mutable.ParMap.empty[ID, Instance]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = {
    f(cmd) match {
      case r@Success(evt: Created) =>
        counter += 1
        r
      //TODO case Success(c: Commit)
      case evt => evt
    }
  }

  private val generator = () => ID(Seq(counter))
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
      Batch.replaceAll,
      Batch.deleteAll
    )
  }.fold(ReadOnly.count)(_ orElse _)
}


