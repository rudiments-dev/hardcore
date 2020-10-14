package dev.rudiments.db.registry

import com.typesafe.config.Config
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes}
import dev.rudiments.hardcore.{Adapter, Command, Event, Message, Skill}
import scalikejdbc._

class H2Adapter(config: Config) extends Adapter[H2Command, H2Event]{

  val schemaName: String = initConnectionPool(config)

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Command): Message = f(cmd)
  val f: Skill = {
    case CheckConnection =>
      try {
        sql"SELECT 1+1".execute().apply()(AutoSession)
        ConnectionOk
      } catch {
        case e: Exception => ConnectionFailure(e)
      }

    case DiscoverSchema(schemaName: String) =>
      try {
        implicit val session: DBSession = AutoSession
        val tables = SQL("SHOW TABLES FROM " + schemaName).map { rs =>
          rs.string("table_name")
        }.toIterable().apply().toSet
        SchemaDiscovered(schemaName, tables)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }

    case DiscoverTable(tableName: String, schemaName: String) =>
      try {
        implicit val session: DBSession = AutoSession
        val columns = SQL("SHOW COLUMNS FROM " + tableName + " FROM " + schemaName).map { rs =>
          Column(
            rs.string("field"),
            ColumnTypes.valueOf(rs.string("type")),
            rs.string("null").equalsIgnoreCase("YES"),
            !rs.string("default").equalsIgnoreCase("NULL"),
            rs.string("key").equalsIgnoreCase("PRI")
          )
        }.toIterable().apply().toSeq
        TableDiscovered(tableName, columns)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }

    case DiscoverReferences(schemaName) =>
      try {
        implicit val session: DBSession = AutoSession
        val references = SQL(
          """
            |SELECT -- table_name (table_columns) REFERENCES ref_name (ref_columns)
            |    fk.constraint_name AS name,
            |    fk.table_name      AS table_name,
            |    fk.column_list     AS table_columns,
            |    pk.table_name      AS ref_name,
            |    pk.column_list     AS ref_columns
            |FROM   information_schema.constraints fk
            |  JOIN information_schema.constraints pk
            |      ON fk.unique_index_name =  pk.unique_index_name
            |     AND pk.constraint_type =    'PRIMARY KEY'
            |WHERE fk.table_schema = ?
            |  AND fk.constraint_type = 'REFERENTIAL'
            |""".stripMargin.trim).bind(schemaName).map { rs =>
          Reference(
            rs.string("name"),
            rs.string("table_name"),
            rs.string("table_columns").split(","),
            rs.string("ref_name"),
            rs.string("ref_columns").split(",")
          )
        }.toIterable().apply().toSet
        ReferencesDiscovered(schemaName, references)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }

  }

  def initConnectionPool(config: Config): String = {
    val driver =    config.getString("driver")
    val url =       config.getString("url")
    val user =      config.getString("user")
    val password =  config.getString("password")
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password)
    config.getString("schema")
  }
}

trait H2Command extends Command
case object CheckConnection extends H2Command
case class DiscoverSchema(name: String) extends H2Command
case class DiscoverTable(tableName: String, schemaName: String) extends H2Command
case class DiscoverReferences(schemaName: String) extends H2Command

trait H2Event extends Event
case object ConnectionOk extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event
case class SchemaDiscovered(name: String, tables: Set[String]) extends H2Event
case class TableDiscovered(name: String, columns: Seq[Column]) extends H2Event
case class ReferencesDiscovered(schemaName: String, references: Set[Reference]) extends H2Event

case class Reference(
  name: String,
  table: String,
  columns: Seq[String],
  refTable: String,
  refColumns: Seq[String]
)