package dev.rudiments.types.registry

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.CRUD.Create
import dev.rudiments.data.{SoftApp, SoftModule}
import dev.rudiments.types._

object TypeRegistry extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()
  private implicit val typeSystem: TypeSystem = TypeSystem()
  implicit val t: Type = typeSystem.asType[Type]

  private val exampleModule = SoftModule("types", "name")

  typeSystem.types.collect {
    case (name, it: Type) => // fails because Thing isn't a Type
      exampleModule.context.adapter(Create(ID(name), t.fromScala(it)))
  }
  val app = new SoftApp(ConfigFactory.load(), exampleModule)
  app.init()
}
