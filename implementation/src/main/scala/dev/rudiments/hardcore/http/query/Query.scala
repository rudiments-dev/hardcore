package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.Predicate
import dev.rudiments.hardcore.types.DTO

case class Query[T <: DTO](parts: Set[Predicate[_]])
