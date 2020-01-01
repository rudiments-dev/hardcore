package dev.rudiments.hardcore.http.query
import scala.reflect.runtime.universe
import universe._

object QueryParser {

  def parse[T : TypeTag](httpQuery: HttpParams): QueryBlueprint[T] = {
    val types: Map[String, universe.Type] = implicitly[TypeTag[T]].tpe.members.filter(!_.isMethod).map { member =>
      member.name.toString.trim -> member.typeSignature
    }.toMap

    val blueprints: Seq[Either[RuntimeException, PredicateBlueprint[_]]] = httpQuery.parts.map { part =>
      val tt:Type = types(part.fieldName)
      val fabrics = possibleQueries(tt)

      fabrics.foldLeft(Option.empty[PredicateBlueprint[_]]){(accum, fabric) => accum.orElse(fabric(part.text)) }
        .toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
    }

    val parts = blueprints.partition(_.isRight)._1.map(_.right.get).toSet
    QueryBlueprint(parts) //todo eh
  }

  private val possibleQueries: Map[universe.Type, Seq[String => Option[PredicateBlueprint[_]]]] = Map(
    typeOf[String] -> Seq(
      EqualsBlueprint.create[String],
      StartsWithBlueprint.create[String]
    ),
    typeOf[Int] -> Seq(
      EqualsBlueprint.create[Int],
      LessBlueprint.create[Int],
      MoreBlueprint.create[Int]
    )
  )
}
