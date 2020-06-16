package dev.rudiments.hardcore.types

import java.util.UUID

import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

case class TypeSystem(name: String, types: Map[String, Type]) extends DTO
object     TypeSystem {
  def apply(name: String, types: Type*): TypeSystem = new TypeSystem(name, types.map(t => t.name -> t).toMap)
}

//todo fix primary keys
case class Type(name: String, fields: Map[String, Field]) extends DTO {
  def constructSoft(arguments: Any*): SoftInstance =
    SoftInstance(
      fields
        .zip(arguments)
        .map { case ((name, _), argument) => name -> argument }
    )(this)

  def softFromHard(hard: Any): SoftInstance = hard match {
    case p: Product => constructSoft(p.productIterator.toSeq.map(wrapComposite): _*)
    case other      => ???
  }

  private def wrapComposite(v: Any): Any = v match {
    case i: Boolean => i
    case i: String => i
    case i: UUID => i
    case i: Byte => i
    case i: Short => i
    case i: Int => i
    case i: Long => i
    case i: Float => i
    case i: Double => i
    case i: BigInt => i
    case i: BigDecimal => i
    case i: Option[_] => i
    //TODO enums
    case m: Map[_, _] => m.mapValues(wrapComposite)
    case i: Iterable[_] => i.map(wrapComposite)
    case p: Product => constructSoft(p.productIterator.toSeq.map(wrapComposite))
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

sealed trait FieldType {}

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