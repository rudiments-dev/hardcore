package dev.rudiments.gates.h2

import dev.rudiments.gates.h2.SizeMultipliers.N
import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class H2GateSpec extends AnyWordSpec with SampleH2Gate with Matchers {
  "should connect on correct credentials" in {
    gate(CheckConnection()) should be (ConnectionOk())
  }

  "should found schema by name" in {
    gate(InspectDB()) should be (InspectedDB(Set(ID[Schema, String]("HELLO2"), ID[Schema, String]("INFORMATION_SCHEMA"), ID[Schema, String]("PUBLIC"))))
  }

  "should discover table by name and schema" in {
    gate.schemas(Read[Schema, Schema](ID[Schema, String]("HELLO2"))) |> [Readen[Schema, Schema]] { s =>
      s.value.tables(Find[Table, Table](All))
    } should be (Found[Table, Table](Map(
      ID[Table, String]("SAMPLE") -> Table("SAMPLE", Seq(
        Column("ID", ColumnTypes.BIGINT, false, true, true),
        Column("NAME", ColumnTypes.VARCHAR(255), false, false, false),
        Column("COMMENT", ColumnTypes.CLOB(2147483647, N), true, false, false)
      )),
      ID[Table, String]("EXAMPLE") -> Table("EXAMPLE", Seq(
        Column("ID",                ColumnTypes.BIGINT,               false, true,  true),
        Column("INT_COLUMN",        ColumnTypes.INT,                  true,  false, false),
        Column("BOOL_COLUMN",       ColumnTypes.BOOLEAN,              true,  false, false),
        Column("TINYINT_COLUMN",    ColumnTypes.TINYINT,              true,  false, false),
        Column("SMALLINT_COLUMN",   ColumnTypes.SMALLINT,             true,  false, false),
        Column("BIGINT_COLUMN",     ColumnTypes.BIGINT,               true,  false, false),
        Column("DECIMAL_COLUMN",    ColumnTypes.DECIMAL(12),          true,  false, false),
        Column("DOUBLE_COLUMN",     ColumnTypes.DOUBLE(17),           true,  false, false),
        Column("REAL_COLUMN",       ColumnTypes.REAL(7),              true,  false, false),
        Column("TIME_COLUMN",       ColumnTypes.TIME(8, true),        true,  false, false),
        Column("DATE_COLUMN",       ColumnTypes.DATE(10),             true,  false, false),
        Column("TIMESTAMP_COLUMN",  ColumnTypes.TIMESTAMP(26, true),  true,  false, false),
        Column("BINARY_COLUMN",     ColumnTypes.BINARY(5),            true,  false, false),
        Column("VARCHAR_COLUMN",    ColumnTypes.VARCHAR(67),          true,  false, false),
        Column("CHAR_COLUMN",       ColumnTypes.CHAR(89),             true,  false, false),
        Column("BLOB_COLUMN",       ColumnTypes.BLOB(10, N),          true,  false, false),
        Column("CLOB_COLUMN",       ColumnTypes.CLOB(10, N),          true,  false, false),
        Column("UUID_COLUMN",       ColumnTypes.UUID(16),             true,  false, false),
        Column("ARRAY_COLUMN",      ColumnTypes.ARRAY(2147483647),    true,  false, false),
        Column("ENUM_COLUMN",       ColumnTypes.ENUM(5),              true,  false, false)
      ))
    )))
  }

  "should discover references of schema" in {
    gate.schemas(Read[Schema, Schema](ID[Schema, String]("HELLO2"))) |> [Readen[Schema, Schema]] { s =>
      s.value.references(Find[FK, FK](All))
    } should be (Found[FK, FK](Map(
      ID[FK, String]("REF_1") -> FK(
        "REF_1",
        TableRef(ID[Table, String]("EXAMPLE"), Seq(ID[Column, String]("BIGINT_COLUMN"))),
        TableRef(ID[Table, String]("SAMPLE"), Seq(ID[Column, String]("ID")))
      )
    )))
  }
}
