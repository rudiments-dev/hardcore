package work.unformed.hardcore.http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import io.circe.{Decoder, Encoder}
import work.unformed.hardcore.dsl._

class CrudRouter[A : Meta : Encoder : Decoder](prefix: String, handler: CommandHandler[A], idDirective: Directive1[ID[A]]) extends Router {
  import work.unformed.hardcore.http.CirceSupport._

  override val routes: Route = pathPrefix(prefix) {
    pathEndOrSingleSlash {
      get {
        complete(s"GET Query on $prefix")
      } ~ post {
        entity(as[A]) { draft =>
          complete(StatusCodes.Created, handler.handle(Create(draft)))
        }
      }
    } ~ idDirective { id =>
      get {
        complete(handler.handle(Read(id)))
      } ~ put {
        entity(as[A]) { newValue =>
          complete(handler.handle(Update(newValue)))
        }
      } ~ delete {
        complete(StatusCodes.NoContent, handler.handle(Delete(id)))
      }
    }
  }
}
