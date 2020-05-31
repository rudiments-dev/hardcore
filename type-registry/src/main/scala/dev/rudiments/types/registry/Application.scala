package dev.rudiments.types.registry

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.CRUD.Create
import dev.rudiments.data.{MemoryAdapter, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.RootRouter
import dev.rudiments.hardcore.types._

object Application extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()
  implicit val t: Type = HardType[Example]

  private val exampleModule = new SoftModule("example", "id")

  exampleModule.cache apply Create(SoftID(1), t.softFromHard(Example(1, "one", Seq("red", "green"))))
  exampleModule.cache apply Create(SoftID(2), t.softFromHard(Example(2, "two", Seq("blue"))))
  exampleModule.cache apply Create(SoftID(3), t.softFromHard(Example(3, "three")))

  new SoftApp(ConfigFactory.load(), exampleModule).init()

  private case class Example(
    id: Long = Defaults.long,
    name: String,
    values: Seq[String] = Seq.empty
  ) extends DTO
}
