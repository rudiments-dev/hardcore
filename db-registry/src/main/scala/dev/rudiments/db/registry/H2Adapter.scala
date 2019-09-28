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

trait H2Event extends Event
case object ConnectionOk extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event
