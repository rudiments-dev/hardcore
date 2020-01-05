package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

private[predicates] sealed trait More[T] extends FieldPredicate[T]

case class IntMore(override val fieldName: String, value: Int) extends More[Int]
case class DoubleMore(override val fieldName: String, value: Double) extends More[Double]

object IntMore {
  private val regexp: Regex = "(\\w+)=more:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntMore] = {
    FieldPredicate.create[Int, IntMore](from, regexp)(IntMore.apply)
  }
}

object DoubleMore {
  private val regexp: Regex = "(\\w+)=more:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleMore] = {
    FieldPredicate.create[Double, DoubleMore](from, regexp)(DoubleMore.apply)
  }
}