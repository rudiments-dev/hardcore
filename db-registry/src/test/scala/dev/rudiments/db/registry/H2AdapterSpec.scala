package dev.rudiments.db.registry

import com.typesafe.config.{Config, ConfigFactory}
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class H2AdapterSpec extends WordSpec with Matchers {

  "should connect on correct credentials" in {
    val config: Config = ConfigFactory.parseMap(Map(
      "driver" -> "org.h2.Driver",
      "url" -> "jdbc:h2:mem:hello",
      "user" -> "user",
      "password" -> "pass",
      "schema" -> "hello"
    ).asJava)
    val adapter: H2Adapter = new H2Adapter(config)
    adapter(CheckConnection) should be (ConnectionOk)
    adapter.schemaName should be ("hello")
  }
}
