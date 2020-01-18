package dev.rudiments.hardcore.types


sealed trait Ref[T <: DTO] {}

sealed trait ID[T <: DTO] extends Ref[T] {}
object ID {
  final case class AutoID [T <: DTO]             ()                              extends ID[T]
  final case class ID0    [T <: DTO]             ()                              extends ID[T]
  final case class ID1    [T <: DTO, K]          (key: K)                        extends ID[T]
  final case class ID2    [T <: DTO, K1, K2]     (key1: K1, key2: K2)            extends ID[T]
  final case class ID3    [T <: DTO, K1, K2, K3] (key1: K1, key2: K2, key3: K3)  extends ID[T]

  final def auto [T <: DTO]            ()                            : ID[T] = AutoID[T]()
  final def apply[T <: DTO]            ()                            : ID[T] = ID0[T]()
  final def apply[T <: DTO, K]         (key: K)                      : ID[T] = ID1[T, K](key)
  final def apply[T <: DTO, K1, K2]    (key1: K1, key2: K2)          : ID[T] = ID2[T, K1, K2](key1, key2)
  final def apply[T <: DTO, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[T] = ID3[T, K1, K2, K3](key1, key2, key3)
}

final case class Instance[T <: DTO](value: T) extends Ref[T]

sealed abstract class SoftRef(val t: Type)
sealed abstract class SoftID(override val t: Type) extends SoftRef(t)
object SoftID {
  final case class SoftID0(override val t: Type) extends SoftID(t)
  final case class SoftID1(override val t: Type, key: Any) extends SoftID(t)
  final case class SoftID2(override val t: Type, key1: Any, key2: Any) extends SoftID(t)
  final case class SoftID3(override val t: Type, key1: Any, key2: Any, key3: Any) extends SoftID(t)

  final def apply(t: Type): SoftID = SoftID0(t)
  final def apply(t: Type, key: Any): SoftID = SoftID1(t, key)
  final def apply(t: Type, key1: Any, key2: Any): SoftID = SoftID2(t, key1, key2)
  final def apply(t: Type, key1: Any, key2: Any, key3: Any): SoftID = SoftID3(t, key1, key2, key3)
}
final case class SoftInstance(override val t: Type, fields: Map[String, Any]) extends SoftRef(t)
