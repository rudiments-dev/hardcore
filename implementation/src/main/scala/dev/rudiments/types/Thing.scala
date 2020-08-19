package dev.rudiments.types

import java.util.UUID

import dev.rudiments.types.hard.{IncompatibleType, ScalaTypes}
import enumeratum.EnumEntry

sealed abstract class Thing(
  val name: String, //TODO package name
  val ascendants: Seq[Thing]
) extends ADT {
  def validate(arg: Any): Either[ValidationError, Any] = Right(arg)
}

case class Abstract(
  override val name: String,
  override val ascendants: Seq[Thing] = Seq.empty
) extends Thing(name, ascendants)

case class OnlyOne(
  override val name: String,
  override val ascendants: Seq[Thing] = Seq.empty,
               value: Instance = Empty
) extends Thing(name, ascendants)


case class Type(
  override val name: String,
               fields: Map[String, ValueSpec],
  override val ascendants: Seq[Thing] = Seq.empty
) extends Thing(name, ascendants) {
  def construct(arguments: Any*): Instance = {
    Instance(
      fields
        .zip(arguments)
        .map { case ((name, f), argument) =>
          name -> (f.validate(argument) match {
            case Left(e) => throw new SoftValidationError(name, e)
            case Right(v) => v
          })
        }
    )(this)
  }

  def constructFromMap(arguments: Map[String, Any]): Instance = {
    Instance(
      fields.map { case (name, field) =>
        name -> (field.validate(arguments(name)) match {
          case Left(e) => throw new SoftValidationError(name, e)
          case Right(v) => v
        })
      }
    )(this)
  }

  def fromScala(hard: Any): Instance = hard match {
    case p: Product => construct(
      p.productIterator.toSeq.zip(fields.values).map { case (vi, fi) => wrapComposite(vi, fi.`type`) }: _*
    )
    case other      => ???
  }

  import ScalaTypes._
  private def wrapComposite(v: Any, f: Thing): Any = (f, v) match {
    case (Plain.Bool,         i: Boolean) => i
    case (Plain.Text(_),      i: String) => i
    case (Plain.UUID,         i: UUID) => i

    case (ScalaByte,          i: Byte) => i
    case (ScalaShort,         i: Short) => i
    case (ScalaInt,           i: Int) => i
    case (ScalaLong,          i: Long) => i
    case (ScalaFloat,         i: Float) => i
    case (ScalaDouble,        i: Double) => i

    case (ScalaBigInteger,    i: BigInt) => i
    case (ScalaBigDecimal,    i: BigDecimal) => i

    case (Plain.Date,         i: java.sql.Date) => i
    case (Plain.Time,         i: java.sql.Time) => i
    case (Plain.Timestamp,    i: java.sql.Timestamp) => i

    case (_, i: Option[_]) => i.map(wrapComposite(_, f))

    case (Index(of, over), m: Map[_, _]) => m.map { case (k, v) => wrapComposite(k, of) -> wrapComposite(v, over) }
    case (List(of), i: Iterable[_]) => i.map(wrapComposite(_, of))

    case (t: Type, p: Product) =>
      t.construct(
        p.productIterator.toSeq
          .zip(t.fields.values)
          .map { case(pi, fi) => wrapComposite(pi, fi.`type`) }: _*
      )

    case (a: Algebraic, v: ADT) =>
      a.descendants.collectFirst {
        case i: Type if i.name == v.productPrefix => wrapComposite(v, i)
        case i: OnlyOne if i.name == v.productPrefix => i
      }.getOrElse {
        throw new RuntimeException(s"Incompatible: value $v and field $f")
      }
    case (a: Algebraic, v: EnumEntry) => a.descendants.collectFirst {
      case i: OnlyOne if i.name == v.entryName => i
    }.getOrElse {
      throw new RuntimeException(s"Incompatible: value $v and field $f")
    }

    case (_: Abstract, t: Thing) => t
    case (_, _) => throw new RuntimeException(s"Incompatible: value $v and field $f")
  }
}

case class Algebraic(
  root: Abstract,
  descendants: Set[Thing],
  override val ascendants: Seq[Thing] = Seq.empty
) extends Thing(root.name, ascendants)

object Nothing extends Type("Nothing", Map.empty)
object Empty extends Instance(Map.empty)(Nothing)
object Anything extends Type("Anything", Map.empty)

sealed abstract class Plain(name: String) extends Thing(name, Seq.empty)
object Plain {
  case object Bool extends Plain("Bool")

  case class Text (
    maxSize: NumberSize
  ) extends Plain("Text")

  case class Number (
    min: NumberSize,
    max: NumberSize,
    format: NumberFormat
  ) extends Plain("Number")

  case object Date extends Plain("Date")
  case object Time extends Plain("Time")
  case object Timestamp extends Plain("Timestamp")

  case object UUID extends Plain("UUID")
}

case class List(of: Thing) extends Thing("List", Seq.empty) {

  override def validate(arg: Any): Either[ValidationError, Any] = arg match {
    case i: Iterable[_] if i.isEmpty => Right(arg)
    case i: Iterable[_] => i.map(of.validate).reduce[Either[ValidationError, Any]] {
      case (Right(a), Right(_)) => Right(a)
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    } match {
      case Right(_) => Right(arg)
      case l: Left[ValidationError, _] => l
    }
    case other => Left(IncompatibleType(of.name, other))
  }
}
case class Index(of: Thing, over: Thing) extends Thing("Index", Seq.empty) {

  override def validate(arg: Any): Either[ValidationError, Any] = arg match {
    case i: Map[_, _] if i.nonEmpty => i.map(v => of.validate(v._1) -> over.validate(v._2)).reduce[(Either[ValidationError, Any], Either[ValidationError, Any])] {
      case ((Right(a), Right(b)), (Right(_), Right(_))) => (Right(a), Right(b))
      case ((Left(e), b), _) => (Left(e), b)
      case ((b, Left(e)), _) => (Left(e), b)
      case (_, (Left(e), b)) => (Left(e), b)
      case (_, (b, Left(e))) => (Left(e), b)
    } match {
      case (Right(_), _) => Right(arg)
      case (l: Left[ValidationError, _], _) => l
    }
    case _: Map[_, _] => Right(arg)
    case other => Left(IncompatibleType(of.name, other))
  }
}


case class ValueSpec (
  `type`: Thing,
  isRequired: Boolean,
  default: Instance = Empty
) extends DTO {
  def validate(arg: Any): Either[ValidationError, Any] = arg match {
    case Some(v) if !isRequired => `type`.validate(v).map(Some(_))
    case None if !isRequired => Right(None)
    case other => `type`.validate(arg)
  }
}


sealed trait NumberSize extends ADT
object NumberSize {
  case class  Big(size: BigDecimal) extends NumberSize
  case object Infinity              extends NumberSize
  case object PositiveInfinity      extends NumberSize
  case object NegativeInfinity      extends NumberSize
}

sealed trait NumberFormat extends ADT
object NumberFormat {
  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}

trait ValidationError extends dev.rudiments.hardcore.Error
class SoftValidationError(field: String, e: ValidationError) extends RuntimeException(s"Validation error on field '$field': $e")
case class RequiredNonEmptyCollection(field: String) extends ValidationError
