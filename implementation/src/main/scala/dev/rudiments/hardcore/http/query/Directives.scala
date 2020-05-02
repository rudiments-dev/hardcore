package dev.rudiments.hardcore.http.query

import akka.http.scaladsl.server.Directive1
import dev.rudiments.hardcore.types.DTO
import akka.http.scaladsl.server.Directives._
import scala.reflect.runtime.universe.TypeTag

object Directives {

  def query[T <: DTO : TypeTag]: Directive1[Query[T]] =
    parameter("query").map(HttpParams.apply).map(QueryParser.parse[T]).map {
      case Left(ex) => throw ex
      case Right(value) => value
    }
}
