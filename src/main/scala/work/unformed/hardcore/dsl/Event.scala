package work.unformed.hardcore.dsl

trait Event[A]

case class Created[A](id: ID[A], value: A) extends Event[A]
case class Result[A](id: ID[A], value: A) extends Event[A]
case class Updated[A](id: ID[A], oldValue: A, newValue: A) extends Event[A]
case class Deleted[A](id: ID[A], value: A) extends Event[A]

case class BatchCreated[A](values: Iterable[A]) extends Event[A]
case class AllDeleted[A]() extends Event[A]


case class FKCreated[R, A](ref: ID[R], values: Iterable[A]) extends Event[A]
case class FKResult[R, A](ref: ID[R], values: Iterable[A]) extends Event[A]
case class FKUpdated[R, A](ref: ID[R], oldValues: Iterable[A], newValues: Iterable[A]) extends Event[A]
case class FKDeleted[R, A](ref: ID[R], values: Iterable[A]) extends Event[A]

case class FKBatchCreated[R, A](values: Map[ID[R], Iterable[A]]) extends Event[A]
case class FKAllDeleted[R, A]() extends Event[A]



trait Error[A] extends Throwable with Event[A]

case class NoHandler[A](command: Command[A]) extends Error[A]
case class Failed[A](command: Command[A], cause: Throwable) extends Error[A]
case class NotImplemented[A](string: String) extends Error[A]
case class Internal[R, A](cause: Error[A]) extends Error[R]

case class NotFound[A](id: ID[A]) extends Error[A]
case class AlreadyExists[A](id: ID[A]) extends Error[A]
case class FailedToCreate[A](id: ID[A], value: A) extends Error[A]
case class FailedToUpdate[A](id: ID[A], value: A) extends Error[A]
case class FailedToDelete[A](id: ID[A]) extends Error[A]

case class FKNotFound[R, A](id: ID[R]) extends Error[A]
case class FKAlreadyExists[R, A](id: ID[R]) extends Error[A]
case class FKFailedToCreate[R, A](id: ID[R], values: Iterable[A]) extends Error[A]
case class FKFailedToUpdate[R, A](id: ID[R], values: Iterable[A]) extends Error[A]
case class FKFailedToDelete[R, A](id: ID[R]) extends Error[A]