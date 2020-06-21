package dev.rudiments.hardcore.types

import java.util.UUID

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

case class TypeSystem(name: String, types: Map[String, Type]) extends DTO
object     TypeSystem {
  def apply(name: String, types: Type*): TypeSystem = new TypeSystem(name, types.map(t => t.name -> t).toMap)
}

class SoftValidationError(e: ValidationError) extends RuntimeException(s"Validation error: $e")

//todo fix primary keys
case class Type(name: String, fields: Map[String, Field]) extends DTO {
  def constructSoft(arguments: Any*): SoftInstance = {
    SoftInstance(
      fields
        .zip(arguments)
        .map { case ((name, t), argument) => name -> validate(t, name, argument) }
        .mapValues {
          case Left(e) => throw new SoftValidationError(e)
          case Right(v) => v
        }
    )(this)
  }

  private def validate(f: Field, name: String, arg: Any): Either[ValidationError, Any] = (f.fieldFlag, arg) match {
    case (FieldFlag.Optional, None) => Right(None)
    case (FieldFlag.Optional, Some(v)) => f.kind.validate(v).map(Some.apply)
    case (FieldFlag.Required, v) => f.kind.validate(v)
    case (FieldFlag.WithDefault, v) => f.kind.validate(v) //TODO use default value if empty
    case (FieldFlag.NonEmpty, v: Iterable[_]) =>
      if(v.isEmpty)
        Left(RequiredNonEmptyCollection(name))
      else
        v.map(f.kind.validate).reduce[Either[ValidationError, Any]] {
          case (Right(a), Right(_)) => Right(a)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        } match {
          case Right(_) => Right(arg)
          case l: Left[ValidationError, _] => l
        }
    case (FieldFlag.CanBeEmpty, v: Iterable[_]) =>
      if(v.isEmpty)
        Right(arg)
      else
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

  def softFromHard(hard: Any): SoftInstance = hard match {
    case p: Product => constructSoft(
      p.productIterator.toSeq.zip(fields).map(i => wrapComposite(i._1, i._2._2.kind)): _*
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
    case (i: EnumEntry, e@Types.Enum(_, values)) => SoftEnum(e, values.indexOf(i.entryName))
    case (m: Map[_, _], Types.Index(of, over)) => m.map {
      case (k, v) => wrapComposite(k, of) -> wrapComposite(v, over)
    }
    case (i: Iterable[_], Types.List(of)) => i.map(wrapComposite(_, of))
    case (p: Product, Types.Reference(t)) => t.constructSoft(p.productIterator.toSeq.zip(t.fields).map(j => wrapComposite(j._1, j._2._2.kind)))
    case (_, _) => throw new RuntimeException(s"Incompatible: value $v and field $f")
  }

  def extract(value: Instance, field: String): Any = value match {
    case s: SoftInstance          => s.fields(field)
    case HardInstance(v: Product) => productField(v, field)
    case other                    => ???
  }

  def productField(p: Product, f: String): Any = {
    fields
      .zipWithIndex
      .filter { case ((n, _), _) => n == f }
      .collectFirst { case ((_, _), i) => p.productElement(i) }
      .get
  }
}

case class Field(kind: FieldType, fieldFlag: FieldFlag) extends DTO

trait ValidationError extends dev.rudiments.hardcore.Error

sealed trait FieldType {
  def validate(arg: Any): Either[ValidationError, Any] = Right(arg)
}

object Types {
  case object Bool        extends FieldType
  case class  Text(
    maxSize: NumberSize
  )                       extends FieldType
  case class  Number(
    min: NumberSize,
    max: NumberSize,
    format: NumberFormat
  )                       extends FieldType
  case object Date        extends FieldType
  case object Time        extends FieldType
  case object Timestamp   extends FieldType

  case object UUID        extends FieldType

  case class Enum(
    name: String,
    values: Seq[String]
  )                       extends FieldType

  case class List(of: FieldType)                    extends FieldType
  case class Index(of: FieldType, over: FieldType)  extends FieldType
  case class Reference(of: Type)                    extends FieldType

  case object Unknown                               extends FieldType
}

sealed trait NumberSize {}
case class  Big(size: BigDecimal) extends NumberSize
case object Infinity              extends NumberSize
case object PositiveInfinity      extends NumberSize
case object NegativeInfinity      extends NumberSize

sealed trait NumberFormat extends EnumEntry
object NumberFormat extends enumeratum.Enum[NumberFormat] {
  override def values: immutable.IndexedSeq[NumberFormat] = findValues

  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}


sealed trait FieldFlag extends EnumEntry
object FieldFlag extends enumeratum.Enum[FieldFlag] {
  override def values: immutable.IndexedSeq[FieldFlag] = findValues

  case object Required    extends FieldFlag
  case object Optional    extends FieldFlag
  case object WithDefault extends FieldFlag

  case object NonEmpty    extends FieldFlag
  case object CanBeEmpty  extends FieldFlag
}

case class RequiredNonEmptyCollection(field: String) extends ValidationError