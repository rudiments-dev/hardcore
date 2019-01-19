package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl.{Filter, ID}
import work.unformed.hardcore.repo.FKWriteRepo

import scala.collection.parallel.mutable

class MemoryFKRepo[R, A] extends FKWriteRepo[R, A] {
  private val content = mutable.ParMap.empty[ID[R], Iterable[A]]

  override def get(id: ID[R]): Iterable[A] = content.getOrElse(id, Iterable.empty)

  override def find(filters: Filter[A]*): (ID[R], Iterable[A]) = ???

  override def count(filters: Filter[A]*): Long = content.size

  override def values(field: String, filters: Filter[A]*): Iterable[Any] = ???

  override def create(ref: ID[R], values: Iterable[A]): Iterable[A] = content.get(ref) match {
    case Some(existing) =>
      throw new IllegalArgumentException(s"[${existing.mkString(", ")}] with key $ref already exists")
    case None =>
      content.put(ref, values)
      content(ref)
  }

  override def delete(ref: ID[R]): Unit = content -= ref

  override def createAll(values: Map[ID[R], Iterable[A]]): Unit = content ++= values

  override def deleteAll(): Unit = content.clear()
}
