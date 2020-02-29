package dev.rudiments.hardcore.types


sealed trait Ref[T] {}

sealed trait ID[T] extends Ref[T] {}
object ID {
  final case class AutoID [T]             ()                              extends ID[T]
  final case class ID0    [T]             ()                              extends ID[T]
  final case class ID1    [T, K]          (key: K)                        extends ID[T]
  final case class ID2    [T, K1, K2]     (key1: K1, key2: K2)            extends ID[T]
  final case class ID3    [T, K1, K2, K3] (key1: K1, key2: K2, key3: K3)  extends ID[T]

  final def auto [T]            ()                            : ID[T] = AutoID[T]()
  final def apply[T]            ()                            : ID[T] = ID0[T]()
  final def apply[T, K]         (key: K)                      : ID[T] = ID1[T, K](key)
  final def apply[T, K1, K2]    (key1: K1, key2: K2)          : ID[T] = ID2[T, K1, K2](key1, key2)
  final def apply[T, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[T] = ID3[T, K1, K2, K3](key1, key2, key3)
}

final case class Instance[T](value: T) extends Ref[T]

sealed abstract class SoftRef(implicit val t: Type)
sealed abstract class SoftID()(override implicit val t: Type) extends SoftRef()(t)
object SoftID {
  final case class SoftID0()(override implicit val t: Type) extends SoftID()(t)
  final case class SoftID1(key: Any)(override implicit val t: Type) extends SoftID()(t)
  final case class SoftID2(key1: Any, key2: Any)(override implicit val t: Type) extends SoftID()(t)
  final case class SoftID3(key1: Any, key2: Any, key3: Any)(override implicit val t: Type) extends SoftID()(t)

  final def apply(implicit t: Type): SoftID = SoftID0()(t)
  final def apply(key: Any)(implicit t: Type): SoftID = SoftID1(key)(t)
  final def apply(key1: Any, key2: Any)(implicit t: Type): SoftID = SoftID2(key1, key2)(t)
  final def apply(key1: Any, key2: Any, key3: Any)(implicit t: Type): SoftID = SoftID3(key1, key2, key3)(t)
}

final case class SoftInstance(fields: Map[String, Any])(override implicit val t: Type) extends SoftRef()(t)
object SoftInstance {
  def apply(fields: Seq[Any])(implicit t: Type): SoftInstance = SoftInstance(t.constructMap(fields:_*))(t)
}
