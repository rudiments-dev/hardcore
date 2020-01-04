package dev.rudiments.hardcore.http.query

case class Query[T](testFunction: T => Option[T] ) {

  def compose(query : Query[T]) : Query[T] = {
    Query { entity => this.testFunction(entity).flatMap(query.testFunction) }
  }

}

object Query {

  def pure[T]: Query[T] = Query { dto: T => Some(dto)}
}
