package dev.rudiments.db.registry

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, Find, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.{IDPath, RootRouter, Router}
import dev.rudiments.hardcore.types.{ID, Type}
import io.circe.{Encoder, Json}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  implicit val t: Type[Schema] = Type[Schema]

  try {
    val config = ConfigFactory.load()
    val db = new DataMemoryAdapter[Schema]
    val discover = new H2Adapter(config.getConfig("db"))
    val service = new H2Service(discover, db)

    service(ReadSchema(discover.schemaName))

    import dev.rudiments.hardcore.http.CirceSupport._
    implicit val columnTypeEncoder: Encoder[ColumnType] = new Encoder[ColumnType] {
      override def apply(a: ColumnType): Json = Encoder.encodeString(a.toString)
    }
    implicit val refEncoder: Encoder[FK] = new Encoder[FK] {
      override def apply(a: FK): Json = Encoder.encodeString(a.toString)
    }

    val port = new ReadOnlyHttpPort[Schema]("schema", IDPath[Schema, String], db)
    new RootRouter(config, port).bind()
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
