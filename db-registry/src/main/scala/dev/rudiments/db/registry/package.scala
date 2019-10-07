package dev.rudiments.db

import dev.rudiments.hardcore.types.DTO

package object registry {
  case class Schema(name: String, tables: Set[Table]) extends DTO {}
  case class Table(name: String, columns: Seq[Column], pk: Seq[Column]) extends DTO
  case class Column(name: String, `type`: ColumnType, nullable: Boolean, default: Boolean, pk: Boolean) extends DTO

  case class FK(from: Table, to: Table, references: Seq[(Column, Column)])

  sealed abstract class ColumnType {}
  object ColumnTypes {
    case object INT                                           extends ColumnType
    case object BOOLEAN                                       extends ColumnType
    case object TINYINT                                       extends ColumnType
    case object SMALLINT                                      extends ColumnType
    case object BIGINT                                        extends ColumnType
    case class  DECIMAL(precision: Int)                       extends ColumnType
    case class  DOUBLE(precision: Int)                        extends ColumnType
    case class  REAL(precision: Int)                          extends ColumnType
    case class  TIME(precision: Int, timeZone: Boolean)       extends ColumnType
    case class  DATE(precision: Int)                          extends ColumnType
    case class  TIMESTAMP(precision: Int, timeZone: Boolean)  extends ColumnType
    case class  BINARY(precision: Int)                        extends ColumnType
    case class  VARCHAR(precision: Int)                       extends ColumnType
    case class  CHAR(precision: Int)                          extends ColumnType
    case class  BLOB(precision: Int, times: SizeMultiplier)   extends ColumnType
    case class  CLOB(precision: Int, times: SizeMultiplier)   extends ColumnType
    case class  UUID(precision: Int)                          extends ColumnType
    case class  ARRAY(precision: Int)                         extends ColumnType
    case class  ENUM(precision: Int)                          extends ColumnType

//    case class GEOMETRY()   extends ColumnType
//    case class INTERVAL()   extends ColumnType

    def valueOf(value: String): ColumnType = {
      val INTpattern =        "^INTEGER\\((\\d+)\\)".r
      val BOOLEANpattern =    "^BOOLEAN\\((\\d+)\\)".r
      val TINYINTpattern =    "^TINYINT\\((\\d+)\\)".r
      val SMALLINTpattern =   "^SMALLINT\\((\\d+)\\)".r
      val BIGINTpattern =     "^BIGINT\\((\\d+)\\)".r
      val DECIMALpattern =    "^DECIMAL\\((\\d+)\\)".r
      val DOUBLEpattern =     "^DOUBLE\\((\\d+)\\)".r
      val REALpattern =       "^REAL\\((\\d+)\\)".r
      val TIMEpattern =       "^TIME\\((\\d+)\\)".r
      val DATEpattern =       "^DATE\\((\\d+)\\)".r
      val TIMESTAMPpattern =  "^TIMESTAMP\\((\\d+)\\)".r
      val BINARYpattern =     "^VARBINARY\\((\\d+)\\)".r
      val VARCHARpattern =    "VARCHAR\\((\\d+)\\)".r
      val CHARpattern =       "CHAR\\((\\d+)\\)".r
      val BLOBpattern =       "BLOB\\((\\d+)\\)".r
      val CLOBpattern =       "CLOB\\((\\d+)\\)".r
      val UUIDpattern =       "UUID\\((\\d+)\\)".r
      val ARRAYpattern =      "ARRAY\\((\\d+)\\)".r
      val ENUMpattern =       "ENUM\\((\\d+)\\)".r
      value match {
        case INTpattern(_) =>         INT
        case BOOLEANpattern(_) =>     BOOLEAN
        case TINYINTpattern(_) =>     TINYINT
        case SMALLINTpattern(_) =>    SMALLINT
        case BIGINTpattern(_) =>      BIGINT
        case DECIMALpattern(p) =>     DECIMAL(p.toInt)
        case DOUBLEpattern(p) =>      DOUBLE(p.toInt)
        case REALpattern(p) =>        REAL(p.toInt)
        case TIMEpattern(p) =>        TIME(p.toInt, true)
        case DATEpattern(p) =>        DATE(p.toInt)
        case TIMESTAMPpattern(p) =>   TIMESTAMP(p.toInt, true)
        case BINARYpattern(p) =>      BINARY(p.toInt)
        case VARCHARpattern(p) =>     VARCHAR(p.toInt)
        case CHARpattern(p) =>        CHAR(p.toInt)
        case BLOBpattern(p) =>        BLOB(p.toInt, SizeMultipliers.N)
        case CLOBpattern(p) =>        CLOB(p.toInt, SizeMultipliers.N)
        case UUIDpattern(p) =>        UUID(p.toInt)
        case ARRAYpattern(p) =>       ARRAY(p.toInt)
        case ENUMpattern(p) =>        ENUM(p.toInt)
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
