package dev.rudiments.gates.h2

import com.typesafe.config.Config
import dev.rudiments.hardcore.ADT
import scalikejdbc.ConnectionPool

case class H2Config(
  driver: String = "org.h2.Driver",
  url: String,
  user: String,
  password: String, //TODO Secure string
  schema: String
) extends ADT {
  def initConnectionPool(): this.type = {
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password)
    this
  }
}

object H2Config {
  def apply(config: Config): H2Config = {
    new H2Config(
      config.getString("driver"),
      config.getString("url"),
      config.getString("user"),
      config.getString("password"),
      config.getString("schema")
    )
  }
}
