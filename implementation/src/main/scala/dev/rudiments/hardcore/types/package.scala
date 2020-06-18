package dev.rudiments.hardcore

package object types {
  trait DTO extends Product

  trait Ref
  trait Instance extends Ref {
    def copy[A](fieldName: String, f: A => Either[Error, A]): Either[Error, Instance] = this match {
      case s: SoftInstance =>
        s.fields.get(fieldName) match {
          case Some(field: A) if field.isInstanceOf[A] =>
            f(field).map { ok =>
              SoftInstance(s.fields + (fieldName -> ok))(s.t)
            }
          case None => Left(FieldNotFound(fieldName))
        }
      case other => ???
    }
  }
  trait ID extends Ref
  trait AutoID extends ID

  case class FieldNotFound(field: String) extends Error
}
