package dev.rudiments.hardcore.http.query

case class QueryBlueprint[T](parts: Set[PredicateBlueprint[_]])
