package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex

private[blueprints] sealed trait Less[T] extends FieldPredicate[T]

case class IntLess(override val fieldName: String, override val value: Int) extends Less[Int]
case class DoubleLess(override val fieldName: String, override val value: Double) extends Less[Double]

object IntLess {
  private val regexp: Regex = "(\\w+)=less:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntLess] = {
    FieldPredicate.create[Int, IntLess](from, regexp)(IntLess.apply)
  }
}

object DoubleLess {
  private val regexp: Regex = "(\\w+)=less:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleLess] = {
    FieldPredicate.create[Double, DoubleLess](from, regexp)(DoubleLess.apply)
  }
}