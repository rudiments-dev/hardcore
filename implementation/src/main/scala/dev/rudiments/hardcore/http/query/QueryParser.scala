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
      val fabrics = possibleQueries(tt)

      fabrics.foldLeft(Option.empty[PredicateBlueprint[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
        .toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
    }

    sequence(blueprints).map(_.toSet).map(QueryBlueprint.apply[E])
  }

  val possibleQueries: Map[universe.Type, Seq[String => Option[PredicateBlueprint[_]]]] = Map(
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
