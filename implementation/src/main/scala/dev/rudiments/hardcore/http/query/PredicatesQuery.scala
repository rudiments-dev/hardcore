package dev.rudiments.hardcore.http.query

import dev.rudiments.domain.Spec
import dev.rudiments.hardcore.http.query.predicates.Predicate

sealed trait Query {
  val spec: Spec
}

object Query {
  def apply(filters: Option[String], spec: Spec): Query = {
    filters match {
      case Some(value) => QueryParser.parse(HttpParams(value), spec)
      case None => Right(PassAllQuery(spec))
    }
  } match {
    case Left(ex) => throw ex
    case Right(value) => value
  }
}

case class PassAllQuery(spec: Spec) extends Query
case class PredicatesQuery(parts: Set[Predicate[_]], spec: Spec) extends Query