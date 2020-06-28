package dev.rudiments.hardcore.types

import enumeratum.EnumEntry

import scala.collection.immutable

case class Field(kind: FieldType, fieldFlag: FieldFlag) extends DTO

sealed trait FieldType extends ADT {
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

  case class List(of: FieldType)                    extends FieldType
  case class Index(of: FieldType, over: FieldType)  extends FieldType
  case class Reference(of: Thing)                   extends FieldType

  case object Unknown                               extends FieldType
}

sealed trait NumberSize extends ADT {}
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

trait ValidationError extends dev.rudiments.hardcore.Error
class SoftValidationError(field: String, e: ValidationError) extends RuntimeException(s"Validation error on field '$field': $e")
case class RequiredNonEmptyCollection(field: String) extends ValidationError
