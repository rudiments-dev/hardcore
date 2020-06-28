package dev.rudiments.hardcore

import scala.reflect.ClassTag

package object types {
  trait DTO extends Product
  trait ADT extends Product

  trait Ref
  trait Instance extends Ref {
    def copy[A : ClassTag](fieldName: String, f: A => Either[Error, A]): Either[Error, Instance] = this match {
      case s: SoftInstance =>
        s.fields.get(fieldName) match {
          case Some(field: A) =>
            f(field).map { ok =>
              SoftInstance(s.fields + (fieldName -> ok))(s.t)
            }
          case None => Left(FieldNotFound(fieldName))
        }
      case other => ???
    }

    def extract[T : ClassTag](field: String): T = this match {
      case soft: SoftInstance => soft.t.fields.get(field) match {
        case Some(_) => soft.fields(field) match {
          case t: T => t
          case other => throw new RuntimeException(s"Field $field has unexpected type: ${other.getClass.getName}, not ${implicitly[ClassTag[T]].runtimeClass.getName}")
        }
        case None => throw new RuntimeException(s"Field $field not present in type ${soft.t.name}")
      }
      case other => ???
    }

    def extractID[T : ClassTag](field: String): ID = this match {
      case soft: SoftInstance => SoftID(soft.extract[Any](field))(soft.t)
      case other => ???
    }
  }
  trait ID extends Ref
  trait AutoID extends ID

  case class FieldNotFound(field: String) extends Error
}
