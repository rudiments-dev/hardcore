package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore._
import io.circe.{Decoder, Encoder, Json}

import scala.reflect.runtime.universe.{Type => SysType, _}
import java.sql.Date

class ScalaRouter[T : TypeTag : Encoder : Decoder](
  val path: Path,
  val id: Predicate,
  val agent: Agent
) extends Router with FailFastCirceSupport {
  override val routes: Route = pathDirective(path) {
      plainId(id) { id =>
        get {
          responseWith(agent(Read(id)))
        } ~ delete {
          responseWith(agent(Delete(id)))
        } ~ entity(as[T]) { raw: T =>
          val data = Data.apply[T](raw)
          post {
            responseWith(agent(Create(id, data)))
          } ~ put {
            responseWith(agent(Update(id, data)))
          }
        }
      } ~ get {
        responseWith(agent(Find(All)))
      }
    }

  implicit def dataEncoder(implicit en: Encoder[T]): Encoder[Data] = en.contramap(_.reconstruct[T]())
  implicit val idEncoder: Encoder[ID] = Encoder.encodeString.contramap(id => id.k.toString)

  def responseWith(event: Out): StandardRoute = event match {
    case Created(_, value) =>        complete(StatusCodes.Created, value)
    case Readen(_, value) =>         complete(StatusCodes.OK, value)
    case Updated(_, _, newValue) =>  complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>            complete(StatusCodes.NoContent)
    case Found(_, values) =>         complete(StatusCodes.OK, values.values)

    case NotFound(_) =>              complete(StatusCodes.NotFound)
    case AlreadyExist(_, _) =>       complete(StatusCodes.Conflict)

    case _: Error =>                 complete(StatusCodes.InternalServerError)
    case _ =>                        complete(StatusCodes.InternalServerError)
  }

  def pathDirective(path: Path): Directive0 = path.ids.map {
    case ID(None) => pathSingleSlash
    case ID(a) => pathPrefix(a.toString)
  }.reduce(_ and _)

  def plainId(p: Predicate): Directive1[ID] = p match {
    case ScalaTypes.ScalaLong =>    pathPrefix(LongNumber).map(l => ID(l))
    case ScalaTypes.ScalaInt =>     pathPrefix(IntNumber) .map(i => ID(i))
    case ScalaTypes.ScalaString =>  pathPrefix(Segment)   .map(i => ID(i))
    case Plain.UUID =>              pathPrefix(JavaUUID)  .map(uuid => ID(uuid))
    case Plain.Date =>              pathPrefix(Segment)   .map(s => ID(Seq(Date.valueOf(s))))
  }
}
