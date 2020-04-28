package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex
import scala.reflect.runtime.universe.TypeTag

private[blueprints] sealed trait EqualsBlueprint[T] extends PredicateBlueprint[T]

case class IntEqualsBlueprint(override val fieldName: String, override val value: Int) extends EqualsBlueprint[Int]
case class StringEqualsBlueprint(override val fieldName: String, override val value: String) extends EqualsBlueprint[String]
case class DoubleEqualsBlueprint(override val fieldName: String, override val value: Double) extends EqualsBlueprint[Double]

object IntEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntEqualsBlueprint] = {
    PredicateBlueprint.create[Int, IntEqualsBlueprint](from, regexp)(IntEqualsBlueprint.apply)
  }
}

object DoubleEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleEqualsBlueprint] = {
    PredicateBlueprint.create[Double, DoubleEqualsBlueprint](from, regexp)(DoubleEqualsBlueprint.apply)
  }
}

object StringEqualsBlueprint {
  private val regexp: Regex = "(\\w+)=eq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringEqualsBlueprint] = {
    PredicateBlueprint.create[String, StringEqualsBlueprint](from, regexp)(StringEqualsBlueprint.apply)
  }
}