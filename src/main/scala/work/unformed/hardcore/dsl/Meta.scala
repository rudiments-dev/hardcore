package work.unformed.hardcore.dsl

class Meta[A] {}

sealed trait Ref[A]
trait ID[A] extends Ref[A] {}
object ID {
  def apply[A, K](id: K): ID[A] = new ID1[A, K](id)
  def apply[A, K1, K2](id1: K1, id2: K2): ID[A] = new ID2[A, K1, K2](id1, id2)
}
case class ID1[A, K](id: K) extends ID[A]
case class ID2[A, K1, K2](id1: K1, id2: K2) extends ID[A]

class Instance[A] extends Ref[A] {}