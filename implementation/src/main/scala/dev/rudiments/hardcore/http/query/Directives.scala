package dev.rudiments.hardcore.http.query

import akka.http.scaladsl.server.Directive1
import dev.rudiments.domain.{Spec, ValueSpec}
import akka.http.scaladsl.server.Directives._
import dev.rudiments.hardcore.{All, Equals, FieldExpression, Less, LessOrEquals, More, MoreOrEquals, ParameterExpression, Predicate, TypedPredicate}

object Directives {

  def query(spec: Spec): Directive1[Query] = parameter("query".?).map(Query.apply(_, spec))

  def typedPredicate(spec: Spec): Directive1[Predicate] = parameterMultiMap.map {
    params =>
      TypedPredicate(
        spec,
        params.flatMap {
          case (param, values) => parseParam(spec, param, values.toSet)
        }.toSeq
      ).asInstanceOf[Predicate]
  }

  def parseParam(spec: Spec, param: String, values: Set[String]): Set[Predicate] = {
    spec.fields.get(param) match {
      case Some(existing) => values.map(v => parsePredicate(existing, param, v)).filter(_ != All)
      case None => Set.empty
    }
  }

  def parsePredicate(field: ValueSpec, param: String, value: String): Predicate = {
    if(value.startsWith("eq:")) {
      Equals(
        FieldExpression(param),
        ParameterExpression(field.parse(value.drop(3)))
      )
    } else if(value.startsWith("gt:")) {
      More(
        FieldExpression(param),
        ParameterExpression(field.parse(value.drop(3)))
      )
    } else if(value.startsWith("gte:")) {
      MoreOrEquals(
        FieldExpression(param),
        ParameterExpression(field.parse(value.drop(4)))
      )
    } else if(value.startsWith("lt:")) {
      Less(
        FieldExpression(param),
        ParameterExpression(field.parse(value.drop(3)))
      )
    } else if(value.startsWith("lte:")) {
      LessOrEquals(
        FieldExpression(param),
        ParameterExpression(field.parse(value.drop(4)))
      )
    } else {
      All
    }
  }
}
