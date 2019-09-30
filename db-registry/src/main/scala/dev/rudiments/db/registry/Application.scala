package dev.rudiments.db.registry

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, Find, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.{IDPath, RootRouter, Router}
import dev.rudiments.hardcore.types.{ID, Type}

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
    val schema = discover(DiscoverSchema(discover.schemaName)) match {
      case SchemaFound(name, tableNames) => Schema(
        name,
        tableNames
          .map(name => discover(DiscoverTable(name, discover.schemaName)) match {
            case TableFound(tableName, columns) => Table(tableName, columns)
            case ConnectionFailure(e) => throw e;
          })
      )
      case ConnectionFailure(e) => throw e;
    }
    db(Create(ID(discover.schemaName), schema))

    logger.trace("found: {}", db(Find(ID(discover.schemaName))))

    import dev.rudiments.hardcore.http.CirceSupport._
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
