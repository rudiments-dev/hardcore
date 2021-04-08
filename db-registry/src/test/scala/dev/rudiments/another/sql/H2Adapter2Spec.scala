package dev.rudiments.another.sql

import dev.rudiments.another.hardcore.{All, Count, Counted, Find, FindAll, Found, FoundAll, ID}
import dev.rudiments.another.sql.SizeMultipliers.N
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import scalikejdbc._

@RunWith(classOf[JUnitRunner])
class H2Adapter2Spec extends AnyWordSpec with Matchers {
  private val driver =    "org.h2.Driver"
  private val url =       "jdbc:h2:mem:hello2"
  private val user =      "user"
  private val password =  "pass"
  Class.forName(driver)
  ConnectionPool.singleton(url, user, password)

  val schemaName = "hello2"

  val adapter: H2Adapter = new H2Adapter

  "should connect on correct credentials" in {
    adapter(CheckConnection()) should be (ConnectionOk())
    schemaName should be ("hello2")
  }

  "should found schema by name" in {
    implicit val session: DBSession = AutoSession
    sql"CREATE SCHEMA hello2".execute().apply()
    sql"SET SCHEMA hello2".execute().apply()

    sql"""CREATE TABLE sample (
         |      id IDENTITY PRIMARY KEY,
         |      name VARCHAR(255) NOT NULL,
         |      comment CLOB
         |)""".stripMargin.execute().apply()

    sql"""CREATE TABLE example (
         |    id                IDENTITY PRIMARY KEY,
         |    int_column        INT,
         |    bool_column       BOOLEAN,
         |    tinyint_column    TINYINT,
         |    smallint_column   SMALLINT,
         |    bigint_column     BIGINT,
         |    decimal_column    DECIMAL(12, 3),
         |    double_column     DOUBLE(12),
         |    real_column       REAL(12),
         |    time_column       TIME,
         |    date_column       DATE,
         |    timestamp_column  TIMESTAMP,
         |    binary_column     BINARY(5),
         |    varchar_column    VARCHAR(67),
         |    char_column       CHAR(89),
         |    blob_column       BLOB(10),
         |    clob_column       CLOB(10),
         |    uuid_column       UUID,
         |    array_column      ARRAY,
         |    enum_column       ENUM('RED', 'GREEN', 'BLUE')
         |)""".stripMargin.execute().apply()

    sql"ALTER TABLE example ADD CONSTRAINT ref_1 FOREIGN KEY (bigint_column) REFERENCES sample (id)".execute().apply()

    adapter(InspectDB()) should be (InspectedDB(Set(ID[Schema](Seq("HELLO2")), ID[Schema](Seq("INFORMATION_SCHEMA")), ID[Schema](Seq("PUBLIC")))))
  }

  "should discover table by name and schema" in {
    val found = adapter.schemas(Find(ID[Schema](Seq("HELLO2"))))
    val s = found.asInstanceOf[Found[Schema]].value

    s.tables(FindAll[Table](All)) should be (FoundAll[Table](
      Map(
        ID[Table](Seq("SAMPLE")) -> Table("SAMPLE", Seq(
          Column("ID", ColumnTypes.BIGINT, false, true, true),
          Column("NAME", ColumnTypes.VARCHAR(255), false, false, false),
          Column("COMMENT", ColumnTypes.CLOB(2147483647, N), true, false, false)
        )),
        ID[Table](Seq("EXAMPLE")) -> Table("EXAMPLE", Seq(
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
      )
    ))
    s.tables(Count()) should be (Counted(2))
  }

  "should discover references of schema" in {
    //TODO
  }
}
