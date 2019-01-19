package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._


trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def strict(id: ID[A]): A = get(id) match {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"Can't find $id")
  }

  def get(id: ID[A]): Option[A]

  def find(query: Query[A]): Result[A]

  def count(filters: Filter[A]*): Long

  def values(field: String, filters: Filter[A]*)
}


trait WriteRepository[A] extends ReadRepository[A] {
  def create(draft: A): A

  def update(value: A): A

  def delete(id: ID[A]): Unit

  def createAll(values: Iterable[A]): Unit = values.foreach(create)

  def deleteAll(): Unit
}


trait SingleRepo[A] extends Repository[A] {
  def get(): A

  def update(value: A): A
}