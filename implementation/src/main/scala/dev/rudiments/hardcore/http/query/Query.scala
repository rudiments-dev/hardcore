package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.predicates.Predicate

case class Query[T](parts: Set[Predicate[_]])
