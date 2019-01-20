package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._

import cats.effect._

trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def get(id: ID[A]): IO[Result[A]]
  def find(query: Query[A]): IO[QueryResult[A]]
  def count(filters: Filter[A]*): IO[Long]
}


trait WriteRepository[A] extends ReadRepository[A] {
  def create(draft: A): IO[Created[A]]
  def update(value: A): IO[Updated[A]]
  def delete(id: ID[A]): IO[Deleted[A]]

  def createAll(values: Iterable[A]): IO[BatchCreated[A]]
  def deleteAll(): IO[AllDeleted[A]]

  def handle(command: Command[A]): Event[A] = command match {
    case _ => NotImplemented("command handler on WriteRepository")
//    case Read(id) => get(id) match {
//      case Left(error) => error
//      case Right(result) => result
//    }
//
//    case Create(value) => create(value) match {
//      case Left(error) => error
//      case Right(created) => created
//    }
//
//    case Update(value) => update(value) match {
//      case Left(error) => error
//      case Right(updated) => updated
//    }
//
//    case Delete(id) => delete(id) match {
//      case Left(error) => error
//      case Right(deleted) => deleted
//    }
//
//    case CreateBatch(values) => createAll(values) match {
//      case Left(error) => error
//      case Right(created) => created
//    }
//
//    case DeleteAll() => deleteAll() match {
//      case Left(error) => error
//      case Right(deleted) => deleted
//    }
  }
}


trait SingleRepo[A] extends Repository[A] {
  def get(): IO[Result[A]]
  def update(value: A): IO[Updated[A]]
}