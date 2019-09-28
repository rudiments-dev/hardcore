package dev.rudiments.db.registry

import com.typesafe.config.Config
import dev.rudiments.hardcore.{Adapter, Command, Event, Skill}
import scalikejdbc._

class H2Adapter(config: Config) extends Adapter[H2Command, H2Event]{

  val schemaName: String = initConnectionPool(config)

  override def isDefinedAt(cmd: H2Command): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: H2Command): H2Event = f(cmd)
  val f: Skill[H2Command, H2Event] = {
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
        SchemaFound(schemaName, tables)
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
            rs.string("null").equalsIgnoreCase("YES")
          )
        }.toIterable().apply().toSeq
        TableFound(tableName, columns)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }

  }

  def initConnectionPool(config: Config): String = {
    val driver = config.getString("driver")
    val url = config.getString("url")
    val user = config.getString("user")
    val password = config.getString("password")
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password)
    config.getString("schema")
  }
}

trait H2Command extends Command
case object CheckConnection extends H2Command
case class DiscoverSchema(name: String) extends H2Command
case class DiscoverTable(tableName: String, schemaName: String) extends H2Command

trait H2Event extends Event
case object ConnectionOk extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event
case class SchemaFound(name: String, tables: Set[String]) extends H2Event
case class TableFound(name: String, columns: Seq[Column]) extends H2Event
