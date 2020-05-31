package dev.rudiments.hardcore.http.query


import dev.rudiments.hardcore.http.query.predicates.Predicate
import dev.rudiments.hardcore.types.Type


case class HttpQuery(parts: Set[Predicate[_]], softType: Type)

object HttpQuery {
  def passAllQuery(softType: Type): HttpQuery = HttpQuery(Set.empty, softType)
}