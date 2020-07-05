package dev.rudiments.types.registry

import java.sql.Timestamp

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.CRUD.Create
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.data.{SoftApp, SoftModule}
import dev.rudiments.hardcore.{Command, Error, Event, Failure, Success}
import dev.rudiments.hardcore.http.PostPort
import dev.rudiments.hardcore.types._
import io.circe.Encoder

object Application extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  implicit val t: Type = ScalaType[Example]

  import akka.http.scaladsl.server.Directives._
  import dev.rudiments.hardcore.http.CirceSupport._
  private val exampleModule = SoftModule("example", "id",
    Seq(
      "sample" -> (ctx => PostPort[Extract, Sample, Event](
        value => Extract(value.what),
        {
          case Extract(id) => ctx.adapter(Find(SoftID(id))) match {
            case Success(Found(_, v)) => Extracted(id, v).toEither
            case Failure(NotFound(_)) => FailedToExtract(id).toEither
          }
        },
        {
          case Success(Extracted(_, v))      =>
            implicit val en: Encoder[Instance] = ctx.encoder
            complete(StatusCodes.OK, v)
          case Failure(FailedToExtract(id))  => complete(StatusCodes.NotFound, id)
        }
      ))
    )
  )

  exampleModule.context.adapter apply Create(SoftID(1), t.fromScala(Example(1, "one", Seq("red", "green"))))
  exampleModule.context.adapter apply Create(SoftID(2), t.fromScala(Example(2, "two", Seq("blue"))))
  exampleModule.context.adapter apply Create(SoftID(3), t.fromScala(Example(3, "three")))

  new SoftApp(ConfigFactory.load(), exampleModule).init()

  private case class Example(
    id: Long = Defaults.long,
    name: String,
    values: Seq[String] = Seq.empty
  ) extends DTO

  private case class Sample(
    what: Long,
    when: Timestamp = Defaults.now
  )

  private case class Extract(id: Long) extends Command
  private case class Extracted(id: Long, value: Instance) extends Event
  private case class FailedToExtract(id: Long) extends Error
}
