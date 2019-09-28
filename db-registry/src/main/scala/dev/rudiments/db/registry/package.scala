package dev.rudiments.db

import dev.rudiments.hardcore.types.DTO

package object registry {
  case class Schema(tables: Set[Table]) extends DTO {}
  case class Table(columns: Seq[Column]) extends DTO
  case class Column(name: String, t: ColumnType, nullable: Boolean) extends DTO

  sealed trait ColumnType {}
  object ColumnTypes {
    case object INT                                           extends ColumnType
    case object BOOLEAN                                       extends ColumnType
    case object TINYINT                                       extends ColumnType
    case object SMALLINT                                      extends ColumnType
    case object BIGINT                                        extends ColumnType
    case object IDENTITY                                      extends ColumnType
    case class  DECIMAL(precision: Int, scale: Int)           extends ColumnType
    case class  DOUBLE(precision: Int)                        extends ColumnType
    case class  REAL(precision: Int)                          extends ColumnType
    case class  TIME(precision: Int, timeZone: Boolean)       extends ColumnType
    case object DATE                                          extends ColumnType
    case class  TIMESTAMP(precision: Int, timeZone: Boolean)  extends ColumnType
    case class  BINARY(precision: Int)                        extends ColumnType
    case object OTHER                                         extends ColumnType
    case class  VARCHAR(precision: Int)                       extends ColumnType
    case class  VARCHAR_IGNORECASE(precision: Int)            extends ColumnType
    case class  CHAR(precision: Int)                          extends ColumnType
    case class  BLOB(precision: Int, times: SizeMultiplier)   extends ColumnType
    case class  CLOB(precision: Int, times: SizeMultiplier)   extends ColumnType
    case object UUID                                          extends ColumnType
    case object ARRAY                                         extends ColumnType
    case class  ENUM(values: String)                          extends ColumnType
//    case class GEOMETRY()   extends ColumnType
//    case class INTERVAL()   extends ColumnType
  }

  sealed trait SizeMultiplier
  object SizeMultipliers {
    case object K extends SizeMultiplier
    case object M extends SizeMultiplier
    case object G extends SizeMultiplier
    case object T extends SizeMultiplier
    case object P extends SizeMultiplier
  }
}
