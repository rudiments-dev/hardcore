package dev.rudiments.hardcore.http.query

import akka.http.scaladsl.server.Directive1
import dev.rudiments.hardcore.types.{DTO, Type}
import akka.http.scaladsl.server.Directives._

import scala.reflect.runtime.universe.TypeTag

object Directives {

  def query(softType: Type): Directive1[HttpQuery] =
    parameter("query").map(HttpParams.apply).map(QueryParser.parse(_, softType)).map {
      case Left(ex) => throw ex
      case Right(value) => value
    }
}
