package dev.rudiments.hardcore

sealed trait Thing {}

sealed trait Predicate extends Thing {}

final case class Type(
  fields: (String, Field)*
) extends Predicate {
  def data(values: Any*): Data = Data(this, values)
}
object Type {
  def of(predicates: (String, Predicate)*): Type = new Type(
    predicates.map((id, p) => id -> Field(p, Required)) :_*
  )
}

final case class Field(
  spec: Predicate,
  kind: FieldKind = Required
) extends Thing

sealed trait FieldKind {}
case object Required extends FieldKind
case class WithDefault(d: Any) extends FieldKind
object Optional extends WithDefault(None)

final class CustomPredicate[A](f: A => Boolean) extends Predicate {}

case object Nothing extends Predicate {}

final case class Data(
  what: Predicate,
  data: Any
) extends Thing