package dev.rudiments.gates.h2

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore._
import io.circe.Encoder

class RegistryRouter(gate: H2Gate)(implicit sEn: Encoder[Schema], tEn: Encoder[Table]) {
  val route: Route = {
    path("health") { complete(StatusCodes.OK) } ~
    pathPrefix("db") {
      post { path("inspect") { doneSchema(gate(InspectDB())) } } ~
      get {
        pathSingleSlash {
          doneSchema(gate(Find[Schema, Schema](All)))
        }
      } ~ pathPrefix(Segment) { schemaName =>
        get {
          pathSingleSlash {
            doneTable(gate(Read[Schema, Schema](ID[Schema, String](schemaName))) |> [Readen[Schema, Schema]] { found =>
              found.value.tables(Find[Table, Table](All))
            })
          } ~ path(Segment) { tableName =>
            doneTable(gate(Read[Schema, Schema](ID[Schema, String](schemaName))) |> [Readen[Schema, Schema]] { found =>
              found.value.tables(Read[Table, Table](ID[Table, String](tableName)))
            })
          }
        }
      }
    }
  }

  import dev.rudiments.gates.h2.H2CirceSupport._
  def doneSchema(out: Out): StandardRoute = out match {
    case evt: Found[Schema, Schema] =>  complete(StatusCodes.OK, evt.content.values)
    case _: InspectedDB =>              complete(StatusCodes.OK)

    case NotFound(_) =>                 complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>         complete(StatusCodes.Conflict)
    case _: Error =>                    complete(StatusCodes.InternalServerError)
    case _ =>                           complete(StatusCodes.InternalServerError)
  }

  def doneTable(out: Out): StandardRoute = out match {
    case evt: Readen[Table, Table] => complete(StatusCodes.OK, evt.value)
    case evt: Found[Table, Table] =>  complete(StatusCodes.OK, evt.content.values)

    case NotFound(_) =>               complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>       complete(StatusCodes.Conflict)
    case _: Error =>                  complete(StatusCodes.InternalServerError)
    case _ =>                         complete(StatusCodes.InternalServerError)
  }
}
