package work.unformed.hardcore.repo

import akka.stream.scaladsl.Sink
import work.unformed.hardcore.dsl._
import cats.effect._

trait EventPersistence {

  val eventMaster: EventStreamer

  val onEvent: PartialFunction[Event[_], Unit]

  eventMaster.subscribe().map(onEvent.apply).runWith(Sink.ignore)(eventMaster.materializer)
}

trait Repository[A] {}


trait ReadRepository[A] extends Repository[A] {
  def get(id: ID[A]): IO[Result[A]]
  def find(query: Query[A]): IO[QueryResult[A]]
  def count(filters: Filter[A]*): IO[Long]
}


trait WriteRepository[A] extends ReadRepository[A] with CommandHandler[A] with EventPersistence {
  def create(draft: A): IO[Created[A]]

  def update(value: A): IO[Updated[A]]

  def delete(id: ID[A]): IO[Deleted[A]]

  def createAll(values: Iterable[A]): IO[BatchCreated[A]]

  def deleteAll(): IO[AllDeleted[A]]

  def getAll(): IO[ResultAll[A]]

  def handle(command: Command[A]): Event[A] = IO[Event[A]] {
    command match {
      case ReadAll() => getAll().attempt.unsafeRunSync() match {
        case Right(result) => result
        case Left(error) => Failed(command, error)

      }
      case Read(id) => get(id).attempt.unsafeRunSync() match {
        case Right(result) => result
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case Create(value) => create(value).attempt.unsafeRunSync() match {
        case Right(created) => created
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case Update(value) => update(value).attempt.unsafeRunSync() match {
        case Right(updated) => updated
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case Delete(id) => delete(id).attempt.unsafeRunSync() match {
        case Right(deleted) => deleted
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case CreateBatch(values) => createAll(values).attempt.unsafeRunSync() match {
        case Right(created) => created
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case DeleteAll() => deleteAll().attempt.unsafeRunSync() match {
        case Right(deleted) => deleted
        case Left(error: Error[A]) => error
        case Left(error) => Failed(command, error)
      }

      case _ => NotImplemented("command handler on WriteRepository")
    }
  }.map { event =>
    eventMaster.publish(event)
    event
  }.unsafeRunSync()

}


trait SingleRepo[A] extends Repository[A] {
  def get(): IO[Result[A]]
  def update(value: A): IO[Updated[A]]
}