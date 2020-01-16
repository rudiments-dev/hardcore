package dev.rudiments.hardcore.http.query
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.hardcore.types.DTO

import scala.reflect.runtime.universe
import universe._

object QueryParser {

  import dev.rudiments.hardcore.http.query.predicates.TypeTransformers._

  def parse[E <: DTO : TypeTag](httpQuery: HttpParams): Either[RuntimeException, Query[E]] = {
    val types: Map[String, universe.Type] = implicitly[TypeTag[E]].tpe.members.filter(!_.isMethod).map { member =>
      member.name.toString.trim -> member.typeSignature
    }.toMap

    val predicates: Seq[Either[RuntimeException, Predicate[_]]] = httpQuery.parts.map { part =>
      val tt: Type = types(part.fieldName)
      recurs(tt, part)
    }

    sequence(predicates).map(_.toSet).map(Query.apply[E])
  }

  private def recurs(fieldType: Type, part: Param):Either[RuntimeException, FieldPredicate[_]] = {
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
      fabrics.foldLeft(Option.empty[FieldPredicate[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
    }

    result.toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
  }

  val fieldPredicates: Map[universe.Type, Seq[String => Option[FieldPredicate[_]]]] = Map(
    typeOf[String] -> Seq(
      StartsWith.create,
      StringEquals.create
    ),
    typeOf[Int] -> Seq(
      IntEquals.create,
      IntLess.create,
      IntMore.create
    )
  )

  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}
