package dev.rudiments.gates.h2

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.gates.h2.ColumnTypes.{ARRAY, BIGINT, BINARY, BLOB, BOOLEAN, CHAR, CLOB, DATE, DECIMAL, DOUBLE, ENUM, INT, REAL, SMALLINT, TIME, TIMESTAMP, TINYINT, UUID, VARCHAR}
import dev.rudiments.hardcore.http.CirceSupport
import io.circe.{Encoder, Printer}
import io.circe.generic.extras.{AutoDerivation, Configuration}

trait H2CirceSupport extends CirceSupport {
  implicit def columnTypeEncoder: Encoder[ColumnType] = {
    case INT => Encoder.encodeString("INT")
    case BOOLEAN => Encoder.encodeString("BOOLEAN")
    case TINYINT => Encoder.encodeString("TINYINT")
    case SMALLINT => Encoder.encodeString("SMALLINT")
    case BIGINT => Encoder.encodeString("BIGINT")

    case DECIMAL(p) => Encoder.encodeString(s"DECIMAL($p)")
    case DOUBLE(p) => Encoder.encodeString(s"DOUBLE($p)")
    case REAL(p) => Encoder.encodeString(s"REAL($p)")

    case TIME(p, z) => Encoder.encodeString(s"TIME($p, $z)")
    case DATE(p) => Encoder.encodeString(s"DATE($p)")
    case TIMESTAMP(p, z) => Encoder.encodeString(s"TIMESTAMP($p, $z)")

    case BINARY(p) => Encoder.encodeString(s"BINARY($p)")

    case VARCHAR(p) => Encoder.encodeString(s"VARCHAR($p)")
    case CHAR(p) => Encoder.encodeString(s"CHAR($p)")
    case BLOB(p, s) => Encoder.encodeString(s"BLOB($p, $s)")
    case CLOB(p, s) => Encoder.encodeString(s"CLOB($p, $s)")
    case UUID(p) => Encoder.encodeString(s"UUID($p)")

    case ARRAY(p) => Encoder.encodeString(s"ARRAY($p)")
    case ENUM(p) => Encoder.encodeString(s"ENUM($p)")
  }
}

object H2CirceSupport extends AutoDerivation with FailFastCirceSupport with H2CirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)
}

