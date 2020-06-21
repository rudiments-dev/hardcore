package dev.rudiments.hardcore.types.registry

import java.sql.Timestamp

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.CRUD.Create
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.data.{SoftApp, SoftModule}
import dev.rudiments.hardcore.{Command, Error, Event}
import dev.rudiments.hardcore.http.PostPort
import dev.rudiments.hardcore.types._
import io.circe.Encoder

object Application extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()

  val typesModule = SoftModule("types", "name")(HardType[Type])
  implicit val t: Type = typesModule.context.t

  typesModule.context.adapter apply Create(SoftID("Example"), t.softFromHard(HardType[Example]))
  typesModule.context.adapter apply Create(SoftID("Sample"), t.softFromHard(HardType[Sample]))

  val app: SoftApp = new SoftApp(ConfigFactory.load(), typesModule)
  app.init()

  private case class Example(
    id: Long = Defaults.long,
    name: String,
    values: Seq[String] = Seq.empty
  ) extends DTO

  private case class Sample(
    what: Long,
    when: Timestamp = Defaults.now
  )
}
