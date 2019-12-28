package dev.rudiments.hardcore.types


sealed trait Ref[T <: DTO] {}

sealed trait ID[T <: DTO] extends Ref[T] {}
object ID {
  final case class AutoID [T <: DTO : HardType]             ()                              extends ID[T]
  final case class ID0    [T <: DTO : HardType]             ()                              extends ID[T]
  final case class ID1    [T <: DTO : HardType, K]          (key: K)                        extends ID[T]
  final case class ID2    [T <: DTO : HardType, K1, K2]     (key1: K1, key2: K2)            extends ID[T]
  final case class ID3    [T <: DTO : HardType, K1, K2, K3] (key1: K1, key2: K2, key3: K3)  extends ID[T]

  final def auto [T <: DTO : HardType]            ()                            : ID[T] = AutoID[T]()
  final def apply[T <: DTO : HardType]            ()                            : ID[T] = ID0[T]()
  final def apply[T <: DTO : HardType, K]         (key: K)                      : ID[T] = ID1[T, K](key)
  final def apply[T <: DTO : HardType, K1, K2]    (key1: K1, key2: K2)          : ID[T] = ID2[T, K1, K2](key1, key2)
  final def apply[T <: DTO : HardType, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[T] = ID3[T, K1, K2, K3](key1, key2, key3)
}

final case class Instance[T <: DTO](value: T) extends Ref[T]
