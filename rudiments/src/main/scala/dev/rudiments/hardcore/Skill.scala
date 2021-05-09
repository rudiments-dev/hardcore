package dev.rudiments.hardcore

import scala.reflect.ClassTag

trait Skill extends PartialFunction[(In, Tx), Out] {
  val signature: ID[In]
  val outcomes: Set[ID[Out]]
  val f: PartialFunction[(In, Tx), Out]

  override def isDefinedAt(in: (In, Tx)): Boolean = f.isDefinedAt(in)
  override def apply(in: (In, Tx)): Out = f.apply(in)
}

class SagaSkill[I <: In : ClassTag, O <: Out : ClassTag](s: (I, Tx) => Out) extends Skill {
  override val signature: ID[In] = ID[In, String](implicitly[ClassTag[I]].runtimeClass.getName)
  override val outcomes: Set[ID[Out]] = Set(ID[Out, String](implicitly[ClassTag[O]].runtimeClass.getName))
  override val f: PartialFunction[(In, Tx), Out] = {
    case (in: I, tx) =>
      tx(in, Started)
      tx(in, s(in, tx))
  }
}
object SagaSkill {
  def apply[I <: In : ClassTag, O <: Out : ClassTag](s: (I, Tx) => Out): SagaSkill[I, O] = new SagaSkill[I, O](s)
  def apply[I <: In : ClassTag, O <: Out : ClassTag](s: I => Out): SagaSkill[I, O] = new SagaSkill[I, O]((in, _) => s(in))
}

case class NotImplemented[I <: In](in: I) extends Error