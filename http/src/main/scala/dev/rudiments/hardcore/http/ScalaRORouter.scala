package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore._
import io.circe.{Encoder, KeyEncoder}

import scala.reflect.runtime.universe.TypeTag

class ScalaRORouter[T : TypeTag](
  val id: Predicate,
  val agent: Agent
) extends Router with CirceSupport {
  implicit val idEncoder: KeyEncoder[ID] = KeyEncoder.encodeKeyString.contramap(id => id.k.toString)
  implicit val valEncoder: Encoder[Map[ID, Thing]] = Encoder.encodeMap[ID, Thing]


  override val routes: Route = {
    path(Segments(1, 128) ~ Slash) { segments =>
      val (a, id) = searchSegment(segments)
      get {
        pathEnd {
          responseWith(
            a(Read(id)) match {
              case Readen(_, ag: Agent) => ag(Find(All))
              case other => other
            }
          )
        }
      }
    } ~ path(Segments(1, 128)) { segments =>
      val (a, id) = searchSegment(segments)
      get {
        pathEnd {
          responseWith(a(Read(id)))
        }
      }
    } ~ get {
      rawPathPrefix(Slash ~ PathEnd) {
        responseWith(agent(Find(All)))
      }
    }
  }

  def responseWith(event: Out): StandardRoute = event match {
    case Readen(_, value: Thing) =>          complete(StatusCodes.OK, value)
    case Found(_, values: Map[ID, Thing]) => complete(StatusCodes.OK, values)

    case NotFound(_) =>        complete(StatusCodes.NotFound)

    case _: Error =>           complete(StatusCodes.InternalServerError)
    case _ =>                  complete(StatusCodes.InternalServerError)
  }

  private def searchSegment(segments: Seq[String]): (Agent, ID) = {
    val ids: Seq[ID] = segments.map(ID)
    val id = ids.last
    val p = Path(ids.dropRight(1) :_*)
    val found = p.ids.foldLeft(agent) { (location, i) =>
      location(Read(i)) match {
        case Readen(_, a: Agent) => a
      }
    }
    (found, id)
  }
}
