package dev.rudiments.db.registry

import com.typesafe.config.{Config, ConfigFactory}
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner
import scalikejdbc._

import collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class H2AdapterSpec extends WordSpec with Matchers {
  val config: Config = ConfigFactory.parseMap(Map(
    "driver" -> "org.h2.Driver",
    "url" -> "jdbc:h2:mem:hello",
    "user" -> "user",
    "password" -> "pass",
    "schema" -> "hello"
  ).asJava)
  val adapter: H2Adapter = new H2Adapter(config)

  "should connect on correct credentials" in {
    adapter(CheckConnection) should be (ConnectionOk)
    adapter.schemaName should be ("hello")
  }

  "should found schema by name" in {
    implicit val session: DBSession = AutoSession
    sql"CREATE SCHEMA hello".execute().apply()
    sql"SET SCHEMA hello".execute().apply()

    sql"""CREATE TABLE sample (
         |      id IDENTITY PRIMARY KEY,
         |      name VARCHAR(255) NOT NULL,
         |      comment CLOB
         |)""".stripMargin.execute().apply()

    sql"""CREATE TABLE example (
         |    id IDENTITY PRIMARY KEY,
         |    sample_id BIGINT NOT NULL,
         |    comment CLOB,
         |    FOREIGN KEY (sample_id) REFERENCES sample (id)
         |)""".stripMargin.execute().apply()

    adapter(DiscoverSchema("hello")) should be (SchemaFound("hello", Set("SAMPLE", "EXAMPLE")))
  }

  "should discover table by name and schema" in {
    adapter(DiscoverTable("sample", "hello")) should be (
      TableFound("sample", Seq(
        Column("ID", ColumnTypes.BIGINT, false),
        Column("NAME", ColumnTypes.VARCHAR(255), false),
        Column("COMMENT", ColumnTypes.CLOB(2147483647, SizeMultipliers.N), true),
      ))
    )
  }
}
