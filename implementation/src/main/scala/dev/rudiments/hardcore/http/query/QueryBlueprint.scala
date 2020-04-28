package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.PredicateBlueprint

case class QueryBlueprint[T](parts: Set[PredicateBlueprint[_]])
