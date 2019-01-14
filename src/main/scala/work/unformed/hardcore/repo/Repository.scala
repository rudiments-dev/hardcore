package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._


trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def get(id: ID[A]): A

  def option(id: ID[A]): Option[A]

  def find(query: Query[A]): Result[A]

  def count(): Long

  def values(field: String, filters: Filter[A]*)
}


trait WriteRepository[A] extends ReadRepository[A] {
  def create(draft: A): A

  def update(value: A): A

  def delete(id: ID[A]): Unit

  def createAll(values: Seq[A]): Unit = values.foreach(create)

  def deleteAll(): Unit
}


trait BackRefRepo[A, B] extends Repository[A] {
  def create(ref: ID[B], values: Set[A]): Set[A]

  def update(ref: ID[B], values: Set[A]): Set[A] = {
    delete(ref)
    create(ref, values)
  }

  def delete(ref: ID[B]): Unit
}


trait SingleRepo[A] extends Repository[A] {
  def get(): A

  def update(value: A): A
}