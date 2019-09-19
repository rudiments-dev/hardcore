package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

sealed trait FieldType {}
object FieldType {
  def apply(t: SysType): FieldType = {
    if      (t =:= typeOf[String]) BasicTypes.Text
    else if (t =:= typeOf[Int] || t =:= typeOf[Double] || t =:= typeOf[Long] || t =:= typeOf[BigDecimal]) BasicTypes.Number
    else if (t =:= typeOf[Timestamp]) BasicTypes.Timestamp
    else if (t =:= typeOf[Date]) BasicTypes.Date
    else if (t =:= typeOf[Time]) BasicTypes.Time
    //TODO add enum
    else BasicTypes.Unknown // TODO add error handling
  }
}

trait DTO extends Product with FieldType

object BasicTypes {
  case object Text      extends FieldType
  case object Number    extends FieldType
  case object Timestamp extends FieldType
  case object Date      extends FieldType
  case object Time      extends FieldType
  case object Enum      extends FieldType
  case object Unknown   extends FieldType
}