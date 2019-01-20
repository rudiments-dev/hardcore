package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._

trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def get(id: ID[A]): Either[Error[A], Result[A]]
  def find(query: Query[A]): Either[Error[A], QueryResult[A]]
  def count(filters: Filter[A]*): Long
}


trait WriteRepository[A] extends ReadRepository[A] {
  def create(draft: A): Either[Error[A], Created[A]]
  def update(value: A): Either[Error[A], Updated[A]]
  def delete(id: ID[A]): Either[Error[A], Deleted[A]]

  def createAll(values: Iterable[A]): Either[Error[A], BatchCreated[A]]
  def deleteAll(): Either[Error[A], AllDeleted[A]]

  def handle(command: Command[A]): Event[A] = command match {
    case Read(id) => get(id) match {
      case Left(error) => error
      case Right(result) => result
    }

    case Create(value) => create(value) match {
      case Left(error) => error
      case Right(created) => created
    }

    case Update(value) => update(value) match {
      case Left(error) => error
      case Right(updated) => updated
    }

    case Delete(id) => delete(id) match {
      case Left(error) => error
      case Right(deleted) => deleted
    }

    case CreateBatch(values) => createAll(values) match {
      case Left(error) => error
      case Right(created) => created
    }

    case DeleteAll() => deleteAll() match {
      case Left(error) => error
      case Right(deleted) => deleted
    }
  }
}


trait SingleRepo[A] extends Repository[A] {
  def get(): Either[Error[A], Result[A]]
  def update(value: A): Either[Error[A], Updated[A]]
}