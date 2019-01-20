package work.unformed.hardcore.repo

import cats.effect.IO
import work.unformed.hardcore.dsl._


trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def strict(id: ID[A]): IO[A] = get(id).map {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"Can't find $id")
  }

  def get(id: ID[A]): IO[Option[A]]

  def find(query: Query[A]): Result[A]

  def count(filters: Filter[A]*): Long

  def values(field: String, filters: Filter[A]*)
}


trait WriteRepository[A] extends ReadRepository[A] {
  def create(draft: A): IO[A]

  def update(value: A): IO[A]

  def delete(id: ID[A]): IO[Unit] //may be IO[int], count of deleted elements

  def createAll(values: Iterable[A]): IO[Unit] = {
    import cats.implicits._
    values.map(create).toList.sequence.map(_ => ())
  }

  def deleteAll(): IO[Unit] //may be IO[int], count of deleted elements
}


trait SingleRepo[A] extends Repository[A] {
  def get(): A

  def update(value: A): A
}