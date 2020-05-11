package dev.rudiments.types.registry

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.SoftCRUD.Create
import dev.rudiments.data.{MemoryAdapter, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.RootRouter
import dev.rudiments.hardcore.types._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {
  logger.info("Starting application")

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  try {
    val config = ConfigFactory.load()
    implicit val t: Type = HardType[Example]
    val db = new MemoryAdapter
    db(Create(SoftID(1), t.softFromHard(Example(1, "one", Seq("red", "green")))))
    db(Create(SoftID(2), t.softFromHard(Example(2, "two", Seq("blue")))))
    db(Create(SoftID(3), t.softFromHard(Example(3, "three"))))

    new RootRouter(config, new ReadOnlyHttpPort("example", "id", db)).bind()
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

  case class Example(
    id: Long = Defaults.long,
    name: String,
    values: Seq[String] = Seq.empty
  ) extends DTO

}
