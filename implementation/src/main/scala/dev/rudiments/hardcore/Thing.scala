package dev.rudiments.hardcore

import dev.rudiments.hardcore.ScalaTypes._
import dev.rudiments.hardcore.Size.{NegativeInfinity, PositiveInfinity}

import scala.reflect.runtime.universe.{Type => SysType, _}

sealed trait Thing {}

sealed trait Ref extends Thing {}
final case class ID(k: Any) extends Ref
final case class Path(ids: ID*) extends Ref
final case class Data(p: Predicate, v: Any) extends Ref {
  def apply(cmd: Command): Event = ???
  def apply(evt: Event): Data = ???

  def reconstruct[T](): T = {
    p match {
      case _: Plain => v.asInstanceOf[T] // validate?
      case Type(_, Some(fullName)) =>
        Class.forName(fullName).getConstructors()(0) //TODO internal classes via $
          .newInstance(v.asInstanceOf[Seq[Object]]: _*).asInstanceOf[T]
      case other => throw new IllegalArgumentException(s"Can't reconstruct from $other")
    }

  }
}
object Data {
  def build[T : TypeTag](args: Any*): Data = {
    val t = Type.build[T]
    Data(t, args)
  }

  def apply[T : TypeTag](value: T): Data = value match {
    case s: String => Data(Plain.Text(s.size), s)
    case i: Int => Data(ScalaInt, i)
    case i: Long => Data(ScalaLong, i)
    case i: BigInt => Data(ScalaBigInteger, i)
    case i: BigDecimal => Data(ScalaBigDecimal, i) //TODO think about level of construction between Data and Type

    case p: Product => build[T](p.productIterator.toList: _*)
  }
}

abstract class Agent(val in: Predicate, val out: Predicate) extends PartialFunction [In, Out] with Ref {
  val f: PartialFunction[In, Out]
  override def isDefinedAt(x: In): Boolean = f.isDefinedAt(x)
  override def apply(x: In): Out = f.apply(x)
}

class Instruction(f: Any => Any) extends Thing {}
sealed trait Expression extends Thing {}
sealed trait Predicate extends Expression {
  def validate(value: Any): Boolean
}
trait Skill extends Expression {}
object Skill {
  def apply(act: PartialFunction[In, Out]): RO = RO(act)
  def apply(act: PartialFunction[In, Out], commit: PartialFunction[Out, Thing]): RW = RW(act, commit)
  def apply(roSkills: RO*): RO = RO(roSkills.map(_.act).reduce(_ orElse _))
  def apply(skills: Skill*): RW = {
    val groupped = skills.groupBy {
      case _: RW => "cmd"
      case _: RO => "evt"
    }
      RW(
        act = skills.map {
          case q: RO => q.act
          case c: RW => c.act
        }.reduce(_ orElse _),

        commit = groupped("cmd").map { case c: RW => c.commit }.reduce(_ orElse _)
      )
  }
}
case class RO(act: PartialFunction[In, Out]) extends Skill {}
case class RW(act: PartialFunction[In, Out], commit: PartialFunction[Out, Thing]) extends Skill {}

final case class List(item: Predicate) extends Predicate {
  override def validate(value: Any): Boolean = true //TODO fix
}
final case class Index(of: Predicate, over: Predicate) extends Predicate {
  override def validate(value: Any): Boolean = true //TODO fix
}

final case class Abstract(fields: Seq[Field] = Seq.empty, fullName: Option[String] = None) extends Predicate {
  override def validate(value: Any): Boolean = {
    (fullName, value) match {
      case (Some(name), Data(Abstract(fields, Some(typeName)), _)) => this.fields == fields && name == typeName
      case (None,       Data(Abstract(fields, None), _))           => this.fields == fields
      //TODO compare with list of partners
      case _ => false
    }
  }
}
final case class Type(fields: Seq[Field] = Seq.empty, fullName: Option[String] = None) extends Predicate {
  override def validate(value: Any): Boolean = {
    (fullName, value) match {
      case (Some(name), Data(Type(fields, Some(typeName)), _)) => this.fields == fields && name == typeName
      case (None,       Data(Type(fields, None), _))           => this.fields == fields
      //TODO compare with list of partners
      case _ => false
    }
  }
}

object Type {
  def build[A : TypeTag]: Predicate = make(typeOf[A])

  def make(sysType: SysType): Predicate = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    plain.getOrElse(this.fullName(symbol), if (sysType <:< typeOf[Any]) {
      makeAlgebraic(symbol)
    } else {
      throw new IllegalArgumentException(s"Scala type not supported: $name")
    })
  }

  def makeAlgebraic(symbol: Symbol): Predicate = {
    val t = symbol.asType
    if(t.isAbstract) {
      Abstract(fieldsOf(t), Some(fullName(t)))
    } else if(t.isModuleClass) {
      AllOf(fieldsOf(t): _*)
    } else if(t.isClass) {
      Type(fieldsOf(t), Some(fullName(t)))
    } else {
      throw new IllegalArgumentException(s"Scala type ${t.name} not algebraic")
    }
  }

  def fieldsOf(t: TypeSymbol): Seq[Field] = {
    val paramLists = t.asClass.primaryConstructor.typeSignature.paramLists
    if(paramLists.isEmpty) {
      Seq.empty
    } else {
      Seq(paramLists.head.collect { case ts: TermSymbol => ifOption(ts) }: _*)
    }
  }

  def ifOption(ts: TermSymbol): Field = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      Field(this.name(ts), make(ts.typeSignature.typeArgs.head), required = false)
    } else {
      Field(this.name(ts), make(ts.typeSignature), required = true)
    }
  }

  def name(s: Symbol): String = s.name.toString.trim
  def fullName(s: Symbol): String = s.fullName.trim
}

case class Field(name: String, p: Predicate, required: Boolean) extends Predicate {
  override def validate(value: Any): Boolean = {
    ???
  }
}

sealed trait Plain extends Predicate {}

object Plain {
  case object Bool extends Plain {
    override def validate(value: Any): Boolean = value match {
      case _: Boolean => true //TODO more checks?
      case _ => false
    }
  }

  case class Text (
    maxSize: Size
  ) extends Plain {
    override def validate(value: Any): Boolean = value match {
      case _: String => true //TODO more checks?
      case _ => false
    }
  }

  case class Number (
    min: Size,
    max: Size,
    format: NumberFormat
  ) extends Plain {
    override def validate(value: Any): Boolean = value match {
      case _: Byte if min == MinByte && max == MaxByte => true
      case _: Short if min == MinShort && max == MaxShort => true
      case _: Int if min == MinInt && max == MaxInt => true
      case _: Long if min == MinLong && max == MaxLong => true
      case _: BigInt if min == NegativeInfinity && max == PositiveInfinity => true
      case _: BigDecimal if min == NegativeInfinity && max == PositiveInfinity => true //TODO more checks?
      case _ => false
    }
  }

  sealed trait Temporal extends Plain {}
  case object Date extends Temporal {
    override def validate(value: Any): Boolean = value match {
      case _: java.sql.Date => true
      case _ => false
    }
  }
  case object Time extends Temporal {
    override def validate(value: Any): Boolean = value match {
      case _:java.sql.Time => true
      case _ => false
    }
  }
  case object Timestamp extends Temporal {
    override def validate(value: Any): Boolean = value match {
      case _:java.sql.Timestamp => true
      case _ => false
    }
  }


  case object UUID extends Plain {
    override def validate(value: Any): Boolean = value match {
      case _: java.util.UUID => true
      case _ => false
    }
  }
}


sealed trait Size
object Size {
  case class  Big(size: BigDecimal) extends Size
  case object Infinity              extends Size
  case object PositiveInfinity      extends Size
  case object NegativeInfinity      extends Size
}

sealed trait NumberFormat
object NumberFormat {
  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}

case object All extends Predicate {
  override def validate(value: Any): Boolean = true
}

sealed trait CompositePredicate extends Predicate {}
case class AllOf(p: Predicate*) extends CompositePredicate {
  override def validate(value: Any): Boolean = {
    p.forall(_.validate(value))
  }
}
case class AnyOf(p: Predicate*) extends CompositePredicate {
  override def validate(value: Any): Boolean = {
    p.exists(_.validate(value))
  }
}
case class OneOf(p: Predicate*) extends CompositePredicate {
  override def validate(value: Any): Boolean = {
    p.count(_.validate(value)) != 1
  }
}