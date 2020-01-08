package dev.rudiments.hardcore.http.query
import dev.rudiments.hardcore.http.query.blueprints._
import dev.rudiments.hardcore.types.DTO

import scala.reflect.runtime.universe
import universe._

object QueryParser {

  import dev.rudiments.hardcore.http.query.blueprints.TypeTransformers._

  def parse[E <: DTO : TypeTag](httpQuery: HttpParams): Either[RuntimeException, QueryBlueprint[E]] = {
    val types: Map[String, universe.Type] = implicitly[TypeTag[E]].tpe.members.filter(!_.isMethod).map { member =>
      member.name.toString.trim -> member.typeSignature
    }.toMap

    val blueprints: Seq[Either[RuntimeException, PredicateBlueprint[_]]] = httpQuery.parts.map { part =>
      val tt: Type = types(part.fieldName)
      recurs(tt, part)
    }

    sequence(blueprints).map(_.toSet).map(QueryBlueprint.apply[E])
  }

  private def recurs(fieldType: Type, part: Param):Either[RuntimeException, PredicateBlueprint[_]] = {
    val result = if (fieldType <:< typeOf[Option[_]])  {
      IsEmpty.create(part.text)
        .orElse(IsDefined.create(part.text))
        .orElse {
          fieldType.typeArgs.headOption.flatMap { tt =>
            recurs(tt, part).toOption.flatMap(OptionValuePredicate.create(part, _))
          }
        }
    } else if (fieldType <:< typeOf[Product]) {
      val p = Param(
        part.text.replaceFirst(part.fieldName + ".", "")
      )
      val relativeTypes: Map[String, universe.Type] = fieldType.members.filter(!_.isMethod).map { member =>
        member.name.toString.trim -> member.typeSignature
      }.toMap
      recurs(relativeTypes(p.fieldName), p).toOption.flatMap(ProductFieldPredicate.create(part.text, _))
    } else {
      val fabrics = fieldPredicates(fieldType)
      fabrics.foldLeft(Option.empty[PredicateBlueprint[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
    }

    result.toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
  }

  val fieldPredicates: Map[universe.Type, Seq[String => Option[FieldPredicateBlueprint[_]]]] = Map(
    typeOf[String] -> Seq(
      StartsWith.create,
      StringEqualsBlueprint.create
    ),
    typeOf[Int] -> Seq(
      IntEqualsBlueprint.create,
      IntLessBlueprint.create,
      IntMoreBlueprint.create
    )
  )

  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}
