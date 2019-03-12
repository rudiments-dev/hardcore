package work.unformed.hardcore.dsl

trait Command[A] extends Event[A]

case class Create[A](value: A) extends Command[A]
case class Read[A](id: ID[A]) extends Command[A]
case class Update[A](value: A) extends Command[A]
case class Delete[A](id: ID[A]) extends Command[A]
case class ReadAll[A]() extends Command[A]
case class CreateBatch[A](values: Iterable[A]) extends Command[A]
case class DeleteAll[A]() extends Command[A]

case class FKCreate[R, A](ref: ID[R], values: Iterable[A]) extends Command[A]
case class FKRead[R, A](ref: ID[R]) extends Command[A]
case class FKUpdate[R, A](ref: ID[R], values: Iterable[A]) extends Command[A]
case class FKDelete[R, A](ref: ID[R]) extends Command[A]

case class FKInsertBatch[R, A](values: Map[ID[R], Iterable[A]]) extends Command[A]
case class FKDeleteAll[R, A]() extends Command[A]


trait CommandHandler[A] {
  def handle(command: Command[A]): Event[A]
}