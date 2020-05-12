package dev.rudiments.hardcore.http.query
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.hardcore.types.Types.Reference
import dev.rudiments.hardcore.types.{DTO, Field, FieldFlag, FieldType, Type, Types}


object QueryParser {

  import dev.rudiments.hardcore.http.query.predicates.TypeTransformers._

  def parse(httpQuery: HttpParams, softType: dev.rudiments.hardcore.types.Type): Either[RuntimeException, Query] = {
    val types: Map[String, Field] = softType.fields

    val predicates: Seq[Either[RuntimeException, Predicate[_]]] = httpQuery.parts.map { part =>
      val tt = types(part.fieldName)
      recurs(tt, part)
    }

    sequence(predicates).map(_.toSet).map(Query(_, softType))
  }

//  def optionPredicate(field: Field, part: Param):Option[FieldPredicate[_]] = {
//      IsEmpty.create(part.text)
//        .orElse(IsDefined.create(part.text))
//        .orElse {
//          fieldPredicate(field, part).flatMap(OptionValuePredicate.create(part, _))
//        }
//    }
//
//  def fieldPredicate(field: Field, part: Param):Option[FieldPredicate[_]] = {
//    val fabrics = fieldPossiblePredicates(field)
//    fabrics.foldLeft(Option.empty[FieldPredicate[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
//  }
//
//  def referencePredicate(ref: Reference, part: Param):Option[FieldPredicate[_]] = {
//    val p = Param(
//      part.text.replaceFirst(part.fieldName + ".", "")
//    )
//    val relativeTypes: Map[String, Field] = ref.of.fields
//
//    recurs(relativeTypes(p.fieldName), p).toOption.flatMap(ProductFieldPredicate.create(part.text, _))
//  }

  private def recurs(field: Field, part: Param):Either[RuntimeException, FieldPredicate[_]] = {

    val result: Option[FieldPredicate[_]] = field.fieldFlag match {
      case FieldFlag.Optional =>
        IsEmpty.create(part.text)
          .orElse(IsDefined.create(part.text))
          .orElse {
            recurs(field.copy(fieldFlag = FieldFlag.Required), part) //todo so dirty clone
              .toOption.flatMap(OptionValuePredicate.create(part, _))
          }
      case _ =>
        field.kind match {
          case Reference(of) =>
            val p = Param(
              part.text.replaceFirst(part.fieldName + ".", "")
            )
            val relativeTypes: Map[String, Field] = of.fields
            recurs(relativeTypes(p.fieldName), p).toOption.flatMap(ProductFieldPredicate.create(part.text, _))
          case _ =>
            val fabrics = fieldPossiblePredicates(field)
            fabrics.foldLeft(Option.empty[FieldPredicate[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
        }
    }
    result.toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
  }

  private def fieldPossiblePredicates(field: Field): Seq[String => Option[FieldPredicate[_]]] =
    field.kind match {
      case Types.Bool => Seq.empty
      case Types.Text(maxSize) => Seq(
        StartsWith.create,
        StringEquals.create
      )
      case Types.Number(min, max, format) => Seq(
        IntEquals.create,
        IntLess.create,
        IntMore.create
      )
      case Types.Date => Seq.empty
      case Types.Time => Seq.empty
      case Types.Timestamp => Seq.empty
      case Types.Enum(name, values) => Seq.empty
      case Types.List(of) => Seq.empty
      case Types.Index(of, over) => Seq.empty
      case Types.Reference(of) => Seq.empty
      case Types.Unknown => Seq.empty
    }


  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}
