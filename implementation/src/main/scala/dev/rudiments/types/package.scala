package dev.rudiments

import scala.reflect.ClassTag
import dev.rudiments.hardcore.Error

package object types {
  trait DTO extends Product
  trait ADT extends Product

  sealed abstract class Ref(implicit val t: Type)
  case class Instance(fields: Map[String, Any])(implicit t: Type) extends Ref()(t) with DTO {
    def copy[A : ClassTag](fieldName: String, f: A => Either[Error, A]): Either[Error, Instance] = this match {
      case s: Instance =>
        s.fields.get(fieldName) match {
          case Some(field: A) =>
            f(field).map { ok =>
              Instance(s.fields + (fieldName -> ok))(s.t)
            }
          case None => Left(FieldNotFound(fieldName))
        }
      case other => ???
    }

    def extract[T : ClassTag](field: String): T = this match {
      case i: Instance => i.t.fields.get(field) match {
        case Some(_) => i.fields(field) match {
          case t: T => t
          case other => throw new RuntimeException(s"Field $field has unexpected type: ${other.getClass.getName}, not ${implicitly[ClassTag[T]].runtimeClass.getName}")
        }
        case None => throw new RuntimeException(s"Field $field not present in type ${i.t.name}")
      }
      case other => ???
    }

    def extractID[T : ClassTag](field: String): ID = this match {
      case i: Instance => ID(i.extract[Any](field))(i.t)
      case other => ???
    }
  }
  object Instance {
    def apply(fields: Any*)(implicit t: Type): Instance = t.construct(fields: _*)
  }

  sealed abstract class ID(implicit t: Type) extends Ref()(t)
  object ID {
    def auto                                  (implicit t: Type): AutoID = AutoID()(t)
    def apply                                 (implicit t: Type): ID = ID0()(t)
    def apply(key: Any)                       (implicit t: Type): ID = ID1(key)(t)
    def apply(key1: Any, key2: Any)           (implicit t: Type): ID = ID2(key1, key2)(t)
    def apply(key1: Any, key2: Any, key3: Any)(implicit t: Type): ID = ID2(key1, key2)(t)

    final case class ID0()                                (override implicit val t: Type) extends ID()(t)
    final case class ID1(key: Any)                        (override implicit val t: Type) extends ID()(t)
    final case class ID2(key1: Any, key2: Any)            (override implicit val t: Type) extends ID()(t)
    final case class ID3(key1: Any, key2: Any, key3: Any) (override implicit val t: Type) extends ID()(t)
  }


  case class AutoID()(override implicit val t: Type) extends ID()(t)

  case class FieldNotFound(field: String) extends Error
}