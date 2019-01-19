package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl.{Filter, ID}

trait FKRepository[R, A] extends Repository [A] {}

trait FKReadRepo[R, A] extends FKRepository[R, A] {
  def get(id: ID[R]): Iterable[A]

  def find(filters: Filter[A]*): (ID[R], Iterable[A])

  def count(filters: Filter[A]*): Long

  def values(field: String, filters: Filter[A]*): Iterable[Any]
}


trait FKWriteRepo[R, A] extends FKReadRepo[R, A] {
  def create(ref: ID[R], values: Iterable[A]): Iterable[A]

  def update(ref: ID[R], values: Iterable[A]): Iterable[A] = {
    delete(ref)
    create(ref, values)
  }

  def delete(ref: ID[R]): Unit

  def createAll(values: Map[ID[R], Iterable[A]]): Unit

  def deleteAll(): Unit
}
