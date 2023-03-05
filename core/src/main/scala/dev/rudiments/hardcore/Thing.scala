package dev.rudiments.hardcore

sealed trait Thing extends Product {}

sealed trait Predicate extends Thing {}

final case class Type(
  fields: (ID, Field)*
) extends Predicate {
  def data(values: Any*): Data = Data(this, values)
}

final case class Field(
  spec: Predicate,
  kind: FieldKind = Required
) extends Thing

sealed trait FieldKind {}
case object Required extends FieldKind
case class WithDefault(d: Any) extends FieldKind
object Optional extends WithDefault(None)

final case class Data(
  what: Predicate,
  data: Any
) extends Thing
