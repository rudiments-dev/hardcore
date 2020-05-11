package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.data.DataCommand
import dev.rudiments.hardcore.http.query.predicates.Predicate


//todo move to soft
case class Query[T](parts: Set[Predicate[_]]) extends DataCommand[T]
