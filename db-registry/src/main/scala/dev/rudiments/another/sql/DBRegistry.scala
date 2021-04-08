package dev.rudiments.another.sql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, pathPrefix}
import akka.http.scaladsl.server.PathMatchers
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.another.hardcore._
import dev.rudiments.another.{In, Out}
import dev.rudiments.hardcore.http.RootRouter
import io.circe.Encoder
import scalikejdbc.ConnectionPool

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DBRegistry extends App with LazyLogging {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  try {
    val config = ConfigFactory.load()
    initConnectionPool(config.getConfig("db"))

    val adapter = new H2Adapter()

    val pipeline: Pipeline[In, In, AutoDbTx] = new Pipeline({ in => (in, new AutoDbTx) })
    val drainage: Drainage[Out, AutoDbTx, Out] = new Drainage({ (out, tx) =>
      tx.session.close()
      out
    })

    val service: Service[In, In, AutoDbTx, Out, Out] = new Service(
      pipeline,
      new CompositeSkill(Seq(adapter, adapter.schemas)),
      drainage
    )

    service(InspectDB())

    import dev.rudiments.hardcore.http.CirceSupport._
    implicit val idEncoder: Encoder[ID[_]] = (a: ID[_]) => Encoder.encodeString(a.keys.mkString("#", "/", ""))
    implicit val columnTypeEncoder: Encoder[ColumnType] = (a: ColumnType) => Encoder.encodeString(a.toString)
//    implicit val refEncoder: Encoder[FK] = (a: FK) => Encoder.encodeString(a.toString)

    val port = new ReadOnlyHttpPort[Table, In, AutoDbTx, Out, String]("schema", service, Seq(
      "inspect" -> EmptyPostPort[In, AutoDbTx, Out](
        InspectDB(),
        service,
        { out => complete(StatusCodes.OK, out.toString) }
      )
    ))
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

  def initConnectionPool(config: Config): String = {
    val driver =    config.getString("driver")
    val url =       config.getString("url")
    val user =      config.getString("user")
    val password =  config.getString("password")
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password)
    config.getString("schema")
  }
}
