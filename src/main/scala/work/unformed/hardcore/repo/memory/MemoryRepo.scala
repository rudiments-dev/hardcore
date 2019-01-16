package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.repo.WriteRepository

import scala.collection.parallel.mutable

class MemoryRepo[A](implicit meta: Meta[A]) extends WriteRepository[A] {
  private val content = mutable.ParMap.empty[ID[A], A]

  override def create(draft: A): A = {
    content.get(draft.identify) match {
      case Some(existing) =>
        throw new IllegalArgumentException(s"$existing with key ${draft.identify} already exists")
      case None =>
        content.put(draft.identify, draft)
        get(draft.identify)
    }
  }

  override def update(value: A): A = {
    content.put(value.identify, value)
    get(value.identify)
  }

  override def delete(id: ID[A]): Unit = content -= id

  override def createAll(values: Seq[A]): Unit = content ++= values.map(v => (v.identify, v))

  override def deleteAll(): Unit = content.clear()

  override def option(id: ID[A]): Option[A] = content.get(id)

  override def find(query: Query[A]): Result[A] = ???

  override def count(): Long = content.size.toLong

  override def values(field: String, filters: Filter[A]*): Unit = ???
}
