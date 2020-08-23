package dev.rudiments.hardcore.http.query

import akka.http.scaladsl.server.Directive1
import dev.rudiments.domain.Spec
import akka.http.scaladsl.server.Directives._

object Directives {

  def query(softType: Spec): Directive1[Query] = parameter("query".?).map(Query.apply(_, softType))
}
