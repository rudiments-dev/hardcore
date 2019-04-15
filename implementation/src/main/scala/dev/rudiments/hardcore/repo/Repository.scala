package dev.rudiments.hardcore.repo

import dev.rudiments.hardcore.dsl._

import cats.effect._

trait Repository[V] {}


trait ReadRepository[K, V] extends Repository[V] {
  def get(key: K): IO[Result[K, V]]
  def find(query: Query[V]): IO[QueryResult[V]]
  def count(filters: Filter[V]*): IO[Long]
}


trait WriteRepository[K ,V] extends ReadRepository[K, V] with DataCommandHandler[K, V] {
  def create(key: K, value: V): IO[Created[K, V]]
  def update(key: K, value: V): IO[Updated[K, V]]
  def delete(key: K): IO[Deleted[K, V]]

  def createAll(values: Map[K, V]): IO[AllCreated[K, V]]
  def deleteAll(): IO[AllDeleted[K, V]]

  def handle(command: DataCommand[K, V]): DataEvent[K, V] = command match {
    case Read(id) => get(id).attempt.unsafeRunSync() match {
      case Right(result) => result
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case Create(key, value) => create(key, value).attempt.unsafeRunSync() match {
      case Right(created) => created
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case Update(key, value) => update(key, value).attempt.unsafeRunSync() match {
      case Right(updated) => updated
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case Delete(id) => delete(id).attempt.unsafeRunSync() match {
      case Right(deleted) => deleted
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case CreateAll(values) => createAll(values).attempt.unsafeRunSync() match {
      case Right(created) => created
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case DeleteAll() => deleteAll().attempt.unsafeRunSync() match {
      case Right(deleted) => deleted
      case Left(error: DataError[K, V]) => error
      case Left(error) => Failed(command, error)
    }

    case _ => ???
  }
}

abstract class PlainRepository[A](implicit meta: Meta[A]) extends WriteRepository[ID[A], A] {
  import dev.rudiments.hardcore.dsl.ID._

  def create(value: A): IO[Created[ID[A], A]] = super.create(value.identify, value)
  def update(value: A): IO[Updated[ID[A], A]] = super.update(value.identify, value)

  def createAll(values: Iterable[A]): IO[AllCreated[ID[A], A]] = super.createAll(
    values.groupBy(_.identify).mapValues(_.head)
  )
}

trait SingleRepo[A] extends Repository[A] {
  def get(): IO[Result[Unit, A]]
  def update(value: A): IO[Updated[Unit, A]]
}