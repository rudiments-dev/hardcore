package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex
import scala.reflect.runtime.universe.TypeTag

private[blueprints] sealed trait EqualsBlueprint[T] extends FieldPredicateBlueprint[T]

case class IntEqualsBlueprint(override val fieldName: String, override val value: Int) extends EqualsBlueprint[Int]
case class StringEqualsBlueprint(override val fieldName: String, override val value: String) extends EqualsBlueprint[String]
case class DoubleEqualsBlueprint(override val fieldName: String, override val value: Double) extends EqualsBlueprint[Double]

object IntEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntEqualsBlueprint] = {
    FieldPredicateBlueprint.create[Int, IntEqualsBlueprint](from, regexp)(IntEqualsBlueprint.apply)
  }
}

object DoubleEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleEqualsBlueprint] = {
    FieldPredicateBlueprint.create[Double, DoubleEqualsBlueprint](from, regexp)(DoubleEqualsBlueprint.apply)
  }
}

object StringEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringEqualsBlueprint] = {
    FieldPredicateBlueprint.create[String, StringEqualsBlueprint](from, regexp)(StringEqualsBlueprint.apply)
  }
}