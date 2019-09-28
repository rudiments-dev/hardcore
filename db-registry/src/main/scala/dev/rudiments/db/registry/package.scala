package dev.rudiments.db

import dev.rudiments.hardcore.types.DTO

package object registry {
  case class Schema(name: String, tables: Set[Table]) extends DTO {}
  case class Table(name: String, columns: Seq[Column]) extends DTO
  case class Column(name: String, t: ColumnType, nullable: Boolean) extends DTO

  sealed abstract class ColumnType {}
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

    def valueOf(value: String): ColumnType = {
      val INTpattern = "^INT\\((\\d+)\\)".r
      val BIGINTpattern = "^BIGINT\\((\\d+)\\)".r
      val VARCHARpattern = "VARCHAR\\((\\d+)\\)".r
      val CLOBpattern = "CLOB\\((\\d+)\\)".r
      value match {
        case INTpattern(_) => INT
        case BIGINTpattern(_) => BIGINT
        case VARCHARpattern(c) => VARCHAR(c.toInt)
        case CLOBpattern(c) => CLOB(c.toInt, SizeMultipliers.N)
      }
    }
  }

  sealed trait SizeMultiplier
  object SizeMultipliers {
    case object N extends SizeMultiplier
    case object K extends SizeMultiplier
    case object M extends SizeMultiplier
    case object G extends SizeMultiplier
    case object T extends SizeMultiplier
    case object P extends SizeMultiplier
  }
}
