package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.data.soft.DataCommand
import dev.rudiments.hardcore.http.query.predicates.Predicate
import dev.rudiments.hardcore.types.Type


case class HttpQuery(parts: Set[Predicate[_]], softType: Type) extends DataCommand
