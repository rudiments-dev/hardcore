package dev.rudiments.hardcore

import dev.rudiments.hardcore.ScalaTypes._
import dev.rudiments.hardcore.{Apply => DevApply}
import dev.rudiments.hardcore.Size.{NegativeInfinity, PositiveInfinity}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{Type => SysType, _}

trait ADT {}
sealed trait Thing extends ADT {}

final case class ID(k: Any) extends Thing {
  def asPath: Path = Path(this)
  def /(id: ID): Path = Path(this, id)
  def /(path: Path): Path = Path(this +: path.ids :_*)

  override def toString: String = "#" + k.toString
}

final case class Path(ids: ID*) extends Thing {
  def /(id: ID): Path = Path(ids :+ id :_*)
  def /(path: Path): Path = Path(ids :++ path.ids :_*)

  def find[T <: Thing : ClassTag](implicit space: Space): T = space.find[T](this)

  def ref(implicit space: Space): Ref = {
    this.find[Thing] match {
      case t: Type => Ref(this, t, None)
      case a: Abstract => Ref(this, a, None)
      case Data(Nothing, Nothing) => Ref(this, Nothing, None)
      case d: Data => Ref(this, d.p, Some(d.v))
      case other => throw new IllegalArgumentException(s"Not supported as Ref: $other")
    }
  }

  def <<(in: In)(implicit space: Space): Out = this.find[Agent] << in
  def <<<(in: In*)(implicit space: Space): Out = this.find[Agent] << DevApply(in)
  def >>(id: ID)(implicit space: Space): Out = this.find[AgentRead] >> id

  override def toString: String = ids.map(_.k).mkString("/", "/", "")
}
object Path {
  val / : Path = Path()
  def apply(s: String): Path = new Path(s.split("/").map(_.trim).map(ID).toIndexedSeq:_*)
}

final case class Data(p: Predicate, v: Any) extends Thing {
  def apply(cmd: Command): Event = ???
  def apply(evt: Event): Data = ???

  def reconstruct[T](implicit space: Space): T = {
    p match {
      case _: Plain => v.asInstanceOf[T] // validate?
      case Ref(_, Type(_, Some(fullName)), _) =>
        reconstruct(fullName, v.asInstanceOf[Seq[Object]]:_*)
      case Type(_, Some(fullName)) =>
        reconstruct(fullName, v.asInstanceOf[Seq[Object]]:_*)
      case other => throw new IllegalArgumentException(s"Can't reconstruct from $other")
    }
  }

  def reconstruct[T](fullName: String, args: Object*): T = {
    Class.forName(fullName).getConstructors()(0) //TODO internal classes via $
      .newInstance(args: _*).asInstanceOf[T]
  }
}
object Data {
  def build[T : TypeTag](args: Any*)(implicit space: Space): Data = {
    val t = Type.build[T]
    Data(t, args)
  }

  def apply[T : TypeTag](value: T)(implicit space: Space): Data = value match {
    case s: String => Data(Plain.Text(s.size), s)
    case i: Int => Data(ScalaInt, i)
    case i: Long => Data(ScalaLong, i)
    case i: BigInt => Data(ScalaBigInteger, i)
    case i: BigDecimal => Data(ScalaBigDecimal, i) //TODO think about level of construction between Data and Type

    case p: Product => build[T](p.productIterator.toList: _*)
  }
}

case class Ref(path: Path, p: Predicate, v: Option[Any] = None) extends Predicate {
  override def validate(value: Any): Boolean = ???
}

abstract class Agent(val in: Predicate, val out: Predicate) extends PartialFunction [In, Out] with Thing {
  val skill: RW
  val f: PartialFunction[In, Out] = {
    case in: In =>
      skill.query(in) match {
        case evt: Event =>
          skill.write(evt)
          evt
        case other => other
      }
  }

  def <<(what: In): Out = f.apply(what)
  def <<?(in: In): Out = skill.query(in)
  def <<!(out: Out): Thing = skill.write(out)

  override def isDefinedAt(x: In): Boolean = f.isDefinedAt(x)
  override def apply(x: In): Out = f.apply(x)
}

abstract class AgentRead(
  override val in: Predicate,
  override val out: Predicate
) extends Agent(in, out) {
  def read(id: ID): Out

  def >>(id: ID): Out = read(id)
}

case class Volatile(p: Predicate, v: Any) extends Thing {
  def as[T : ClassTag]: T = v match {
    case t: T => t
    case other => throw new IllegalArgumentException(s"'$other' is not ${implicitly[ClassTag[T]].runtimeClass.getName}")
  }
}

sealed trait Expression extends Thing {}
sealed trait Predicate extends Expression {
  def validate(value: Any): Boolean
}
trait Skill extends Expression {}
object Skill {
  def apply(query: PartialFunction[In, Out]): RO = RO(query)
  def apply(query: PartialFunction[In, Out], write: PartialFunction[Out, Thing]): RW = RW(query, write)
  def apply(roSkills: RO*): RO = RO(roSkills.map(_.query).reduce(_ orElse _))
  def apply(skills: Skill*): RW = {
    val groupped = skills.groupBy {
      case _: RW => "cmd"
      case _: WO => "cmd"
      case _: RO => "evt"
    }
      RW(
        query = skills.map {
          case q: RO => q.query
          case c: RW => c.query
        }.reduce(_ orElse _),

        write = groupped("cmd").map {
          case c: RW => c.write
          case c: WO => c.write
        }.reduce(_ orElse _)
      )
  }
}
case class RO(query: PartialFunction[In, Out]) extends Skill {}
case class RW(query: PartialFunction[In, Out], write: PartialFunction[Out, Thing]) extends Skill {}
case class WO(write: PartialFunction[Out, Thing]) extends Skill {}
object NoSkill extends RW(
  query = {
    case in =>
      NotImplemented(in)
      throw new IllegalArgumentException("Not implemented")
  },
  write = {
    case _ => throw new IllegalArgumentException("Not implemented")
  }
)

final case class List(item: Predicate) extends Predicate {
  override def validate(value: Any): Boolean = value match {
    case i: Iterable[_] => i.forall(item.validate)
    case _ => false
  }
}
final case class Index(of: Predicate, over: Predicate) extends Predicate {
  override def validate(value: Any): Boolean = value match {
    case m: Map[_, _] => m.forall { case (k, v) => of.validate(k) && over.validate(v) }
    case _ => false
  }
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
  def init(implicit space: Space): Unit = {
    space << Create(ID("types"), new Memory(ScalaString, All)) match {
      case Created(_, mem: Memory) =>       initSequence(mem)
      case AlreadyExist(_, mem: Memory) =>  initSequence(mem)
    }
  }

  private def initSequence(mem: Memory)(implicit space: Space): Unit = {
    mem << DevApply(
      plain.map { case (name, t) => Create(ID(name), t) }.toSeq
    )
    build[Thing]
    build[Message]
    build[In]
    build[Command]
    build[Query]
    build[Out]
    build[Event]
    build[Report]
    build[Error]
    build[CRUD]
  }

  def ref[A : TypeTag](implicit space: Space): Ref =
    Path("types") >> ID(this.name(typeOf[A].typeSymbol)) match {
      case Readen(id, t: Type) => Ref(ID("types") / id, t, None)
      case Readen(id, Data(Nothing, Nothing)) => Ref(ID("types") / id, Nothing, None)
      case Readen(id, Data(p, v)) => Ref(ID("types") / id, p, Some(v))
      case NotFound(id) =>
        throw new IllegalArgumentException(s"$id not initialized")
    }

  def build[A : TypeTag](implicit space: Space): Predicate = getOrMake(typeOf[A])

  def getOrMake(sysType: SysType)(implicit space: Space): Predicate = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    plain.getOrElse(name, if (sysType <:< typeOf[Map[_, _]]) {
      Index(getOrMake(sysType.typeArgs.head), getOrMake(sysType.typeArgs.last))
    } else if (sysType <:< typeOf[Iterable[_]]) {
      List(getOrMake(sysType.typeArgs.head))
    } else if (sysType <:< typeOf[ADT]) {
      makeAlgebraic(symbol)
    } else if (sysType =:= typeOf[Any]) {
      All
    } else {
      makeAlgebraic(symbol)
      //throw new IllegalArgumentException(s"Scala type not supported: $name")
    })
  }

  def makeAlgebraic(symbol: Symbol)(implicit space: Space): Predicate = {
    val t = symbol.asType
    val nameOfT = this.name(t)
    val id = ID(nameOfT)
    val path = ID("types") / id
    Path("types") >> id match {
      case Readen(_, existing: Predicate) => Ref(path, existing)
      case Readen(_, Data(Nothing, Nothing)) => Ref(path, Nothing, None)
      case Readen(_, Data(p, v)) => Ref(path, p, Some(v))
      case NotFound(_) =>
        if(t.isModuleClass) {
          val f = fieldsOf(t)
          val a = if(f.isEmpty) {
            new Data(Nothing, Nothing)
          } else {
            new Data(AllOf(f: _*), f.map(_ => Nothing))
          }
          Path("types") << Create(id, a)
          Ref(path, a.p, Some(a.v))
        } else {
          val a = if (t.isAbstract) {
            Abstract(fieldsOf(t), Some(fullName(t)))
          } else if (t.isClass) {
            Type(fieldsOf(t), Some(fullName(t)))
          } else {
            throw new IllegalArgumentException(s"Scala type ${t.name} not algebraic")
          }
          Path("types") << Create(id, a)
          t.asClass.knownDirectSubclasses.map { s => makeAlgebraic(s) }
          Ref(path, a)
        }
    }
  }

  def fieldsOf(t: TypeSymbol)(implicit space: Space): Seq[Field] = {
    val paramLists = t.asClass.primaryConstructor.typeSignature.paramLists
    if(paramLists.isEmpty) {
      Seq.empty
    } else {
      Seq(paramLists.head.collect { case ts: TermSymbol => ifOption(ts) }: _*)
    }
  }

  def ifOption(ts: TermSymbol)(implicit space: Space): Field = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      Field(this.name(ts), getOrMake(ts.typeSignature.typeArgs.head), required = false)
    } else {
      Field(this.name(ts), getOrMake(ts.typeSignature), required = true)
    }
  }

  def name(s: Symbol): String = s.name.toString.trim
  def fullName(s: Symbol): String = s.fullName.trim
}

case class Field(name: String, p: Predicate, required: Boolean) extends Predicate {
  override def validate(value: Any): Boolean = p.validate(value)
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
case object Nothing extends Predicate {
  override def validate(value: Any): Boolean =
    value == Nothing || value == None
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