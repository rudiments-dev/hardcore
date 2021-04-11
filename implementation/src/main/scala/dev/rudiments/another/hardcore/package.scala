package dev.rudiments.another

import scala.reflect.ClassTag

package object hardcore {

  sealed trait Identifier extends ADT {}

  case class ID[T: ClassTag](keys: Seq[Any]) extends Identifier {
    lazy val is: String = ofType

    def toPair: (String, ID[T]) = {
      ofType -> this
    }

    def ofType: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  }
  case class Path(ids: Seq[ID[_]]) extends Identifier {
    def toMap: Map[String, ID[_]] = ids.map(_.toPair).toMap

    def hasID[T: ClassTag]: Option[ID[T]] = ids.collectFirst { case id: ID[T] => id }

    def after[T: ClassTag]: Path = {
      val of = implicitly[ClassTag[T]].runtimeClass.getSimpleName
      if(ids.isEmpty) throw new IllegalArgumentException("Path should not be empty!")
      val dropped = ids.dropWhile { _.is != of }.drop(1)
      if (dropped.isEmpty) EmptyPath
      else Path(dropped)
    }
  }

  object EmptyPath extends Path(Seq.empty)


  trait PF extends PartialFunction[(In, Tx), Out] {
    val signature: Seq[(ID[In], ID[Out])]
    val f: PartialFunction[(In, Tx), Out]

    override def isDefinedAt(x: (In, Tx)): Boolean = f.isDefinedAt(x)
    override def apply(x: (In, Tx)): Out = f.apply(x)
  }

  class TxSkill[I <: In : ClassTag, T <: Tx : ClassTag, O <: Out : ClassTag](g: (I, T) => Out) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = Seq(
      ID[In](Seq(implicitly[ClassTag[I]].runtimeClass.getName)) -> ID[Out](Seq(implicitly[ClassTag[O]].runtimeClass.getName))
    )

    override val f: PartialFunction[(In, Tx), Out] = {
      case (in: I, tx: T) => g(in, tx)
    }
  }

  class SagaSkill[I <: In : ClassTag, T <: LogTx : ClassTag, O <: Out : ClassTag](g: PartialFunction[I, Out]) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = Seq(
      ID[In](Seq(implicitly[ClassTag[I]].runtimeClass.getName)) -> ID[Out](Seq(implicitly[ClassTag[O]].runtimeClass.getName))
    )

    override val f: PartialFunction[(In, Tx), Out] = {
      case (in: I, tx: T) =>
        tx.log :+ in -> TxStart
        val out = g(in)
        tx.log :+ in -> out
        out
    }
  }

  class CompositeSkill(skills: Seq[PF]) extends PF {
    override val signature: Seq[(ID[In], ID[Out])] = skills.flatMap(_.signature)

    override val f: PartialFunction[(In, Tx), Out] = {
      skills.map(_.f).reduce(_ orElse _) //TODO flatten in case of CompositeSkill?
    }
  }
}
