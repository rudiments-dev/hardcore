package dev.rudiments.hardcore.types

import java.util.UUID
import enumeratum.EnumEntry

sealed trait Thing extends ADT {
  val name: String
}
case class LateInit(name: String) extends Thing

case class Singleton(name: String) extends Thing with Instance // Only 1 instance can exists
case class Declaration(name: String) extends Thing // No instances can exists

//todo fix primary keys
case class Type(name: String, fields: Map[String, Field]) extends Thing {
  def construct(arguments: Any*): Instance = {
    SoftInstance(
      fields
        .zip(arguments)
        .map { case ((name, t), argument) =>
          name -> (validate(t, name, argument) match {
            case Left(e) => throw new SoftValidationError(name, e)
            case Right(v) => v
          })
        }
    )(this)
  }

  def constructFromMap(arguments: Map[String, Any]): Instance = {
    SoftInstance(
      fields.map { case (name, field) =>
        name -> (validate(field, name, arguments(name)) match {
          case Left(e) => throw new SoftValidationError(name, e)
          case Right(v) => v
        })
      }
    )(this)
  }

  private def validate(f: Field, name: String, arg: Any): Either[ValidationError, Any] = (f.fieldFlag, arg) match {
    case (FieldFlag.Optional, None) => Right(None)
    case (FieldFlag.Optional, Some(v)) => f.kind.validate(v).map(Some.apply)
    case (FieldFlag.Required, v) => f.kind.validate(v)
    case (FieldFlag.WithDefault, v) => f.kind.validate(v) //TODO use default value if empty
    case (flag, v: Iterable[_]) if v.isEmpty => flag match {
      case FieldFlag.NonEmpty => Left(RequiredNonEmptyCollection(name))
      case FieldFlag.CanBeEmpty => Right(arg)
    }
    case (_, v: Iterable[_]) =>
      v.map(f.kind.validate).reduce[Either[ValidationError, Any]] {
        case (Right(a), Right(_)) => Right(a)
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      } match {
        case Right(_) => Right(arg)
        case l: Left[ValidationError, _] => l
      }
    case (_, _) => f.kind.validate(arg)
  }

  def fromScala(hard: Any): Instance = hard match {
    case p: Product => construct(
      p.productIterator.toSeq.zip(fields.values).map { case (vi, fi) => wrapComposite(vi, fi.kind) }: _*
    )
    case other      => ???
  }

  private def wrapComposite(v: Any, f: FieldType): Any = (v, f) match {
    case (i: Boolean, Types.Bool) => i
    case (i: String, Types.Text(_)) => i
    case (i: UUID, Types.UUID) => i
    case (i: Byte, ScalaTypes.ScalaByte) => i
    case (i: Short, ScalaTypes.ScalaShort) => i
    case (i: Int, ScalaTypes.ScalaInt) => i
    case (i: Long, ScalaTypes.ScalaLong) => i
    case (i: Float, ScalaTypes.ScalaFloat) => i
    case (i: Double, ScalaTypes.ScalaDouble) => i
    case (i: BigInt, ScalaTypes.ScalaBigInteger) => i
    case (i: BigDecimal, ScalaTypes.ScalaBigDecimal) => i
    case (i: Option[_], _) => i.map(wrapComposite(_, f))
    case (m: Map[_, _], Types.Index(of, over)) => m.map {
      case (k, v) => wrapComposite(k, of) -> wrapComposite(v, over)
    }
    case (i: Iterable[_], Types.List(of)) => i.map(wrapComposite(_, of))
    case (p: Product, Types.Reference(t: Type)) =>
      t.construct(
        p.productIterator.toSeq
          .zip(t.fields.values)
          .map { case(pi, fi) => wrapComposite(pi, fi.kind) }: _*
      )
    case (i: EnumEntry, Types.Reference(of: Enum)) => SoftEnum(of, of.candidates.map(_.name).indexOf(i.entryName))
    case (_, _) => throw new RuntimeException(s"Incompatible: value $v and field $f")
  }
}

case class Algebraic(name: String, root: Declaration, candidates: Seq[Thing]) extends Thing {
  def by(name: String): Option[Thing] = candidates.collectFirst {
    case t: Thing if t.name == name => t
  }
}
class Enum(name: String, root: Declaration, values: Seq[Singleton]) extends Algebraic(name, root, values)