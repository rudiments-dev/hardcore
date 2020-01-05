package dev.rudiments.hardcore.http.query.predicates

object TypeTransformers {
  sealed trait Transformer[A, B] {
    def transform(source: A) : B
  }

  implicit case object toInt extends Transformer[String, Int] {
    override def transform(source: String): Int = source.toInt
  }

  implicit case object pure extends Transformer[String, String] {
    override def transform(source: String): String = source
  }
}
