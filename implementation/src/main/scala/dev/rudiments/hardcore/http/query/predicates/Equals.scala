package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

private[predicates] sealed trait Equals[T] extends FieldPredicate[T]

case class IntEquals(override val fieldName: String, value: Int) extends Equals[Int]
case class StringEquals(override val fieldName: String, value: String) extends Equals[String]
case class DoubleEquals(override val fieldName: String, value: Double) extends Equals[Double]

class BooleanEquals(override val fieldName: String, value: Boolean) extends Equals[Boolean]
case class IsTrue(override val fieldName: String) extends BooleanEquals(fieldName, true)
case class IsFalse(override val fieldName: String) extends BooleanEquals(fieldName, false)

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

object BooleanEquals {
  private val regexp: Regex = "(\\w+)=(true|false)".r

  def create(from: String): Option[BooleanEquals] = {
    FieldPredicate.create[String, BooleanEquals](from, regexp)(func = {
      case (field, "true") => IsTrue(field)
      case (field, "false") => IsFalse(field)
      case _ => ???
    })
  }
}