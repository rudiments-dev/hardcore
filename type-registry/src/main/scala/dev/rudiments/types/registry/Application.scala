package dev.rudiments.types.registry

import java.sql.{Date, Time, Timestamp}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.CRUD.Create
import dev.rudiments.hardcore.data.DataMemoryAdapter
import dev.rudiments.hardcore.http.RootRouter
import dev.rudiments.hardcore.types._
import dev.rudiments.types.registry.module.TypeHttpPort
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {
  logger.info("Starting application")

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  try {
    val config = ConfigFactory.load()
    val db = new DataMemoryAdapter[Type]
    db(Create(ID("CompatibleDTO"), HardType[CompatibleDTO]))
    db(Create(ID("CompatiblePlainDTO"), HardType[CompatiblePlainDTO]))

    new RootRouter(config, TypeHttpPort("types", db)).bind()
  } catch {
    case e: Throwable =>
      logger.error("Error while initializing app, shutdown", e)
      actorSystem.terminate().onComplete {
        case Success(t) => logger.info("Terminated {}", t)
        case Failure(err) =>
          logger.error("Termination failed with error", err)
          sys.exit(-1)
      }
  }

}
