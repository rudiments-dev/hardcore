package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.PredicateBlueprint
import dev.rudiments.hardcore.types.DTO

case class QueryBlueprint[T <: DTO](parts: Set[PredicateBlueprint[_]])
