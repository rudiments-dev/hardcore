package dev.rudiments.hardcore.repo

import dev.rudiments.hardcore.dsl._

import cats.effect._

trait Repository[V] {}


trait ReadRepository[K, V] extends Repository[V] {
  def get(key: K): IO[Result[K, V]]
  def find(query: Query[V]): IO[QueryResult[V]]
  def count(filters: Filter[V]*): IO[Long]
}


trait WriteRepository[K ,V] extends ReadRepository[K, V] with PartialFunction[Command, Event] {
  def create(key: K, value: V): IO[Created[K, V]]
  def update(key: K, value: V): IO[Updated[K, V]]
  def delete(key: K): IO[Deleted[K, V]]

  def createAll(values: Map[K, V]): IO[AllCreated[K, V]]
  def deleteAll(): IO[AllDeleted[K, V]]

  override def apply(command: Command):Event = handle(command)
  override def isDefinedAt(x: Command): Boolean = handle.isDefinedAt(x)

  val io: PartialFunction[Command, IO[Event]] = {
    case c:Create[K, V] =>  create(c.key, c.value)
    case c:Read[K, V] =>    get(c.key)
    case c:Update[K, V] =>  update(c.key, c.value)
    case c:Delete[K, V] =>  delete(c.key)

    case c:CreateAll[K, V] =>  createAll(c.values)
    case _:DeleteAll[K, V] =>  deleteAll()
  }

  val handle: PartialFunction[Command, Event] = io.andThen(handleIO)

  private def handleIO(action: IO[Event]): Event = {
    action.attempt.unsafeRunSync() match {
      case Right(created) => created
      case Left(error: Error) => error
      case Left(error) => Internal(error)
    }
  }
}

abstract class PlainRepository[A](implicit meta: Meta[A]) extends WriteRepository[ID[A], A] {
  import dev.rudiments.hardcore.dsl.ID._

  def create(value: A): IO[Created[ID[A], A]] = create(value.identify, value)
  def update(value: A): IO[Updated[ID[A], A]] = update(value.identify, value)

  def createAll(values: Iterable[A]): IO[AllCreated[ID[A], A]] = createAll(
    values.groupBy(_.identify).mapValues(_.head)
  )

}

trait SingleRepo[A] extends Repository[A] {
  def get(): IO[Result[Unit, A]]
  def update(value: A): IO[Updated[Unit, A]]
}