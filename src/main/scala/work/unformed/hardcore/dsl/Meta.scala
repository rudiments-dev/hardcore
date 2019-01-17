package work.unformed.hardcore.dsl

trait Meta[A] {
  def identify(value: A): ID[A]
}

object Meta {
  def apply[A](f: A => ID[A]): Meta[A] = new Meta[A] {
    override def identify(value: A): ID[A] = f(value)
  }
}

sealed trait Ref[A]
trait ID[A] extends Ref[A] {}
object ID {
  def apply[A, K](id: K): ID[A] = new ID1[A, K](id)
  def apply[A, K1, K2](id1: K1, id2: K2): ID[A] = new ID2[A, K1, K2](id1, id2)

  implicit class IDOps[A](value: A) {
    def identify(implicit meta: Meta[A]): ID[A] = meta.identify(value)
  }
}
case class ID1[A, K](id: K) extends ID[A]
case class ID2[A, K1, K2](id1: K1, id2: K2) extends ID[A]

class Instance[A] extends Ref[A] {}