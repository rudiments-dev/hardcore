package dev.rudiments.app

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.concurrent.ExecutionContext
import io.circe.{Decoder, Encoder}

object Example extends App with LazyLogging {
  val config = ConfigFactory.load()

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  val exampleAgent = new Memory(Type.build[In], Type.build[Out])

  implicit val en: Encoder[Body] = deriveEncoder[Body]
  implicit val de: Decoder[Body] = deriveDecoder[Body]
  logger.info("App almost ready, binding server")

  new RootRouter(
    config,
    new ScalaRouter[Body](
      Path(ID("example")),
      ScalaTypes.ScalaString,
      exampleAgent
    )
  ).bind()
}
