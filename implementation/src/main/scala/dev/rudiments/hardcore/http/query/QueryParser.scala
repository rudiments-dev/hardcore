package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.hardcore.types.Types.{Reference, UUID}
import dev.rudiments.hardcore.types._


object QueryParser {

  import dev.rudiments.hardcore.http.query.predicates.TypeTransformers._

  def parse(httpParams: HttpParams, softType: dev.rudiments.hardcore.types.Type): Either[RuntimeException, Query] = {
    if (httpParams.query.isEmpty) {
      Right(PassAllQuery(softType))
    } else {
      val types: Map[String, Field] = softType.fields

      val predicates: Seq[Either[RuntimeException, Predicate[_]]] = httpParams.parts.map { part =>
        val tt = types(part.fieldName)
        recurs(tt, part)
      }

      sequence(predicates).map(_.toSet).map(PredicatesQuery(_, softType))
    }
  }

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
          case Reference(of: Type) =>
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
      case Types.Bool => Seq(BooleanEquals.create)
      case Types.Text(maxSize) => Seq(
        StringStartsWith.create,
        StringEquals.create,
        StringEndsWith.create,
        StringContains.create
      )
      case Types.Number(min, max, NumberFormat.Integer) => Seq(
        IntEquals.create,
        IntLess.create,
        IntMore.create,
        IntLessOrEquals.create,
        IntMoreOrEquals.create
      )
      case Types.Number(min, max, NumberFormat.Float) => Seq(
        DoubleEquals.create,
        DoubleLess.create,
        DoubleMore.create,
        DoubleLessOrEquals.create,
        DoubleMoreOrEquals.create
      )
      case Types.Number(min, max, NumberFormat.Decimal) => Seq.empty
      case Types.Date => Seq.empty
      case Types.Time => Seq.empty
      case Types.Timestamp => Seq.empty
      case Types.List(of) => Seq.empty
      case Types.Index(of, over) => Seq.empty
      case Types.Reference(of: Type) => Seq.empty
      case Types.Reference(of: Enum) => Seq.empty
      case Types.Unknown => Seq.empty
      case UUID => Seq.empty
    }


  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}
