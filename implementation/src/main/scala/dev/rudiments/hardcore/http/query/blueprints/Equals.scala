package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex
import scala.reflect.runtime.universe.TypeTag

private[blueprints] sealed trait Equals[T] extends FieldPredicate[T]

case class IntEquals(override val fieldName: String, override val value: Int) extends Equals[Int]
case class StringEquals(override val fieldName: String, override val value: String) extends Equals[String]
case class DoubleEquals(override val fieldName: String, override val value: Double) extends Equals[Double]

object IntEquals {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntEquals] = {
    FieldPredicate.create[Int, IntEquals](from, regexp)(IntEquals.apply)
  }
}

object DoubleEquals {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleEquals] = {
    FieldPredicate.create[Double, DoubleEquals](from, regexp)(DoubleEquals.apply)
  }
}

object StringEquals {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringEquals] = {
    FieldPredicate.create[String, StringEquals](from, regexp)(StringEquals.apply)
  }
}