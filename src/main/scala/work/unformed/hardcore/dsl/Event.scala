package work.unformed.hardcore.dsl

trait Event[A]

case class Created[A](value: A) extends Event[A]
case class Updated[A](value: A) extends Event[A]
case class Deleted[A](value: A) extends Event[A]


trait Command[A] extends Event[A]

case class Create[A](value: A) extends Command[A]
case class Update[A](value: A) extends Command[A]
case class Delete[A](id: ID[A]) extends Command[A]

case class RefInsert[A](values: Set[A]) extends Command[A]
case class RefDelete[A, B](ref: ID[B]) extends Command[A]

case class RefUpdate[A, B](ref: ID[B], values: Set[A])