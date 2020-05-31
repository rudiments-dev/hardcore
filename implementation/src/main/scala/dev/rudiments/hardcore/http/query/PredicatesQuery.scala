package dev.rudiments.hardcore.http.query


import dev.rudiments.hardcore.http.query.predicates.Predicate
import dev.rudiments.hardcore.types.Type

sealed trait Query {
  val softType: Type
}

object Query {
  def apply(filters: Option[String], softType: Type): Query = {
    filters match {
      case Some(value) => QueryParser.parse(HttpParams(value), softType)
      case None => Right(PassAllQuery(softType))
    }
  } match {
    case Left(ex) => throw ex
    case Right(value) => value
  }
}

case class PassAllQuery(softType: Type) extends Query
case class PredicatesQuery(parts: Set[Predicate[_]], softType: Type) extends Query