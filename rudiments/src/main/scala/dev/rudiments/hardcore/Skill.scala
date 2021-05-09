package dev.rudiments.hardcore

import scala.reflect.ClassTag

trait Skill extends PartialFunction[(In, Tx), Out] {
  val signature: ID[In]
  val outcomes: Set[ID[Out]]
  val f: PartialFunction[(In, Tx), Out]

  override def isDefinedAt(in: (In, Tx)): Boolean = f.isDefinedAt(in)
  override def apply(in: (In, Tx)): Out = f.apply(in)

  def |>[I <: In : ClassTag, O1 <: Out : ClassTag, O2 <: Out : ClassTag](g: O1 => O2): Skill = new AfterSkill(this)(g)
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

class AfterSkill[I <: In : ClassTag, O1 <: Out : ClassTag, O2 <: Out : ClassTag](skill: Skill)(g: O1 => O2) extends Skill {
  override val signature: ID[In] = skill.signature
  override val outcomes: Set[ID[Out]] = skill.outcomes + ID[Out, String](implicitly[ClassTag[O2]].runtimeClass.getName)
  override val f: PartialFunction[(In, Tx), Out] = {
    case (in: I, tx: Tx) => skill.f(in, tx) match {
      case o: O1 => g(o)
      case other => other
    }
  }
}

class OptionSkill[T, I <: In : ClassTag, O1 <: Out : ClassTag, O2 <: Out : ClassTag](opt: I => Option[T], s: (I, Tx, T) => O1, n: (I, Tx) => O2) extends Skill {
  override val signature: ID[In] = ID[In, String](implicitly[ClassTag[I]].runtimeClass.getName)
  override val outcomes: Set[ID[Out]] = Set(
    ID[Out, String](implicitly[ClassTag[O1]].runtimeClass.getName),
    ID[Out, String](implicitly[ClassTag[O2]].runtimeClass.getName)
  )
  override val f: PartialFunction[(In, Tx), Out] = {
    case (in: I, tx) =>
      tx(in, Started)
      opt(in) match {
        case Some(found) => tx(in, s(in, tx, found))
        case None => n(in, tx)
      }
  }
}

case class NotImplemented[I <: In](in: I) extends Error