package dev.rudiments.db.registry

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.{ReadOnlyHttpPort, SoftCache}
import dev.rudiments.hardcode.sql.schema.{ColumnType, FK}
import dev.rudiments.hardcore.http.RootRouter
import dev.rudiments.domain.{Spec, Domain}
import io.circe.{Encoder, Json}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()
  private implicit val domain: Domain = Domain()

  implicit val spec: Spec = domain.makeFromScala[Spec, Schema]

  try {
    val config = ConfigFactory.load()
    val cache = new SoftCache
    val discover = new H2Adapter(config.getConfig("db"))
    val service = new H2Service(discover, cache)

    service(ReadSchema(discover.schemaName))

    import dev.rudiments.hardcore.http.CirceSupport._
    implicit val columnTypeEncoder: Encoder[ColumnType] = new Encoder[ColumnType] {
      override def apply(a: ColumnType): Json = Encoder.encodeString(a.toString)
    }
    implicit val refEncoder: Encoder[FK] = new Encoder[FK] {
      override def apply(a: FK): Json = Encoder.encodeString(a.toString)
    }

    val port = new ReadOnlyHttpPort("schema", "name", cache)
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
