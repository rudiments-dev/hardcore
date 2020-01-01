package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.EqualsBlueprint.regexp
import dev.rudiments.hardcore.types.FieldType

import scala.util.matching.Regex
import scala.reflect.runtime.universe._

abstract class PredicateBlueprint[T: TypeTag] {
  val tRef: Type = typeOf[T]

  val t: FieldType = FieldType.apply(typeOf[T])
  val fieldName: String
  val value: T
}

object PredicateBlueprint {
  def create[T, F <: PredicateBlueprint[T]](from: String, regex: Regex)(func: (String, T) => F)
                                           (implicit tr: TypeTransformers.Transformer[String, T]):Option[F] = {
    regex.findFirstMatchIn(from).map { value =>
      func(value.group(1), tr.transform(value.group(2)))
    }
  }
}

case class EqualsBlueprint[T: TypeTag](override val fieldName: String, override val value: T) extends PredicateBlueprint[T]
case class StartsWithBlueprint[T: TypeTag](override val fieldName: String, override val value: T) extends PredicateBlueprint[T]
case class LessBlueprint[T: TypeTag](override val fieldName: String, override val value: T) extends PredicateBlueprint[T]
case class MoreBlueprint[T: TypeTag](override val fieldName: String, override val value: T) extends PredicateBlueprint[T]

object EqualsBlueprint {
  private val regexp: Regex = "(\\w)=eq:(.*)".r

  def create[T : TypeTag](from: String)(implicit tr: TypeTransformers.Transformer[String, T]): Option[EqualsBlueprint[T]] = {
    PredicateBlueprint.create[T, EqualsBlueprint[T]](from, regexp)(EqualsBlueprint.apply)
  }
}

object StartsWithBlueprint {
  private val regexp: Regex = "(\\w)=starts:(.*)".r

  def create[T : TypeTag](from: String)(implicit tr: TypeTransformers.Transformer[String, T]): Option[StartsWithBlueprint[T]] = {
    PredicateBlueprint.create[T, StartsWithBlueprint[T]](from, regexp)(StartsWithBlueprint.apply)
  }
}

object LessBlueprint {
  private val regexp: Regex = "(\\w)=less:(.*)".r

  def create[T : TypeTag](from: String)(implicit tr: TypeTransformers.Transformer[String, T]): Option[LessBlueprint[T]] = {
    PredicateBlueprint.create[T, LessBlueprint[T]](from, regexp)(LessBlueprint.apply)
  }
}

object MoreBlueprint {
  private val regexp: Regex = "(\\w)=more:(.*)".r

  def create[T : TypeTag](from: String)(implicit tr: TypeTransformers.Transformer[String, T]): Option[MoreBlueprint[T]] = {
    PredicateBlueprint.create[T, MoreBlueprint[T]](from, regexp)(MoreBlueprint.apply)
  }
}

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