package dev.rudiments.hardcore.http.query

import dev.rudiments.domain._
import dev.rudiments.hardcore.http.query.predicates._

object QueryParser {

  import dev.rudiments.hardcore.http.query.predicates.TypeTransformers._

  def parse(httpParams: HttpParams, spec: dev.rudiments.domain.Spec): Either[RuntimeException, Query] = {
    if (httpParams.query.isEmpty) {
      Right(PassAllQuery(spec))
    } else {
      val types: Map[String, ValueSpec] = spec.fields

      val predicates: Seq[Either[RuntimeException, Predicate[_]]] = httpParams.parts.map { part =>
        val tt = types(part.fieldName)
        recurs(tt, part)
      }

      sequence(predicates).map(_.toSet).map(PredicatesQuery(_, spec))
    }
  }

  private def recurs(field: ValueSpec, part: Param):Either[RuntimeException, FieldPredicate[_]] = {

    val result: Option[FieldPredicate[_]] = if(field.isRequired) {
      field.thing match {
        case of: Spec =>
          val p = Param(
            part.text.replaceFirst(part.fieldName + ".", "")
          )
          val relativeTypes: Map[String, ValueSpec] = of.fields
          recurs(relativeTypes(p.fieldName), p).toOption.flatMap(ProductFieldPredicate.create(part.text, _))
        case _ =>
          val fabrics = fieldPossiblePredicates(field)
          fabrics.foldLeft(Option.empty[FieldPredicate[_]]) { (accum, fabric) => accum.orElse(fabric(part.text)) }
      }
    } else {
      IsEmpty.create(part.text)
        .orElse(IsDefined.create(part.text))
        .orElse {
          recurs(field.copy(isRequired = true), part) //todo so dirty clone
            .toOption.flatMap(OptionValuePredicate.create(part, _))
        }
    }

    result.toRight(left = new RuntimeException(s"unsupported format: ${part.text}"))
  }

  private def fieldPossiblePredicates(field: ValueSpec): Seq[String => Option[FieldPredicate[_]]] =
    field.thing match {
      case Plain.Bool => Seq(BooleanEquals.create)
      case Plain.Text(maxSize) => Seq(
        StringStartsWith.create,
        StringEquals.create,
        StringEndsWith.create,
        StringContains.create
      )
      case Plain.Number(min, max, NumberFormat.Integer) => Seq(
        IntEquals.create,
        IntLess.create,
        IntMore.create,
        IntLessOrEquals.create,
        IntMoreOrEquals.create
      )
      case Plain.Number(min, max, NumberFormat.Float) => Seq(
        DoubleEquals.create,
        DoubleLess.create,
        DoubleMore.create,
        DoubleLessOrEquals.create,
        DoubleMoreOrEquals.create
      )
      case Plain.Number(min, max, NumberFormat.Decimal) => Seq.empty
      case Plain.Date => Seq.empty
      case Plain.Time => Seq.empty
      case Plain.Timestamp => Seq.empty

      case List(of) => Seq.empty
      case Index(of, over) => Seq.empty

      case Plain.UUID => Seq.empty
    }


  private def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, scala.List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}
