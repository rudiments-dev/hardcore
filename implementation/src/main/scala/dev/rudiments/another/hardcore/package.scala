package dev.rudiments.another

import scala.reflect.ClassTag

package object hardcore {
  case class ID[T](keys: Seq[Any]) extends ADT


  trait PF extends PartialFunction[(In, Tx), Out] {
    val signature: Seq[(ID[In], ID[Out])]
    val f: PartialFunction[(In, Tx), Out]

    override def isDefinedAt(x: (In, Tx)): Boolean = f.isDefinedAt(x)
    override def apply(x: (In, Tx)): Out = f.apply(x)
  }

  class TxSkill[I <: In : ClassTag, T <: Tx : ClassTag, O <: Out : ClassTag](g: (I, T) => O) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = Seq(
      ID[In](implicitly[ClassTag[I]].runtimeClass.getName) -> ID[Out](implicitly[ClassTag[O]].runtimeClass.getName)
    )

    override val f: PartialFunction[(In, Tx), Out] = {
      case (in: I, tx: T) => g(in, tx)
    }
  }

  class SagaSkill[I <: In : ClassTag, T <: Tx : ClassTag, O <: Out : ClassTag](g: PartialFunction[I, Out]) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = Seq(
      ID[In](implicitly[ClassTag[I]].runtimeClass.getName) -> ID[Out](implicitly[ClassTag[O]].runtimeClass.getName)
    )

    override val f: PartialFunction[(In, Tx), Out] = {
      case (in: I, tx: T) =>
        //TODO start to log
        g(in)
        //TODO complete to log
    }
  }

  class CompositeSkill(skills: Seq[PF]) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = skills.flatMap(_.signature)

    override val f: PartialFunction[(In, Tx), Out] = {
      skills.map(_.f).reduce(_ orElse _) //TODO flatten in case of CompositeSkill?
    }
  }
}
