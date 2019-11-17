package dev.rudiments.db.sql

import com.typesafe.config.Config
import dev.rudiments.hardcore.{Adapter, Skill}
import scalikejdbc._

class QueryH2Adapter(config: Config) extends Adapter[Query, QueryResult] {
  initConnectionPool(config)

  override def isDefinedAt(cmd: Query): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Query): QueryResult = f(cmd)

  val f: Skill[Query, QueryResult] = {
    case Query(_, TableExpression(t, alias), _, _) =>
      implicit val session: AutoSession.type = AutoSession
      ResultSet(
        SQL(s"SELECT * FROM $t $alias").toMap().list().apply()
      )
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