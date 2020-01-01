package dev.rudiments.hardcore.http.query

abstract class Query[T] {

  def test(entity: T): Option[T]

}
