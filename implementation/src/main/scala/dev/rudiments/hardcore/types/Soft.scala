package dev.rudiments.hardcore.types

sealed abstract class Soft(implicit val t: Type)

sealed abstract class SoftRef(implicit t: Type) extends Soft()(t) with Ref

sealed abstract class SoftID(implicit t: Type) extends SoftRef()(t) with ID
object                SoftID {
  final case class SoftAutoID()                             (implicit t: Type) extends SoftID()(t) with AutoID
  final case class SoftID0()                                (implicit t: Type) extends SoftID()(t)
  final case class SoftID1(key: Any)                        (implicit t: Type) extends SoftID()(t)
  final case class SoftID2(key1: Any, key2: Any)            (implicit t: Type) extends SoftID()(t)
  final case class SoftID3(key1: Any, key2: Any, key3: Any) (implicit t: Type) extends SoftID()(t)

  final def auto                                  (implicit t: Type): SoftID = SoftAutoID()(t)
  final def apply                                 (implicit t: Type): SoftID = SoftID0()(t)
  final def apply(key: Any)                       (implicit t: Type): SoftID = SoftID1(key)(t)
  final def apply(key1: Any, key2: Any)           (implicit t: Type): SoftID = SoftID2(key1, key2)(t)
  final def apply(key1: Any, key2: Any, key3: Any)(implicit t: Type): SoftID = SoftID3(key1, key2, key3)(t)
}

final case class SoftInstance(fields: Map[String, Any])(implicit t: Type) extends SoftRef()(t) with Instance
object           SoftInstance {
  def apply(fields: Seq[Any])(implicit t: Type): SoftInstance = SoftInstance(t.constructMap(fields:_*))(t)
}

final case class SoftEnum(enum: Types.Enum, index: Int)