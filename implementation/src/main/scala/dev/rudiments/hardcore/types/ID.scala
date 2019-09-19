package dev.rudiments.hardcore.types

import scala.reflect.ClassTag


sealed trait Ref[T <: DTO] {}

sealed trait ID[T <: DTO] extends Ref[T] {}
object ID {
  final case class ID0[T <: DTO : ClassTag]             ()                              extends ID[T]
  final case class ID1[T <: DTO : ClassTag, K]          (key: K)                        extends ID[T]
  final case class ID2[T <: DTO : ClassTag, K1, K2]     (key1: K1, key2: K2)            extends ID[T]
  final case class ID3[T <: DTO : ClassTag, K1, K2, K3] (key1: K1, key2: K2, key3: K3)  extends ID[T]

  final def apply[T <: DTO : ClassTag]                                          : ID[T] = ID0[T]
  final def apply[T <: DTO : ClassTag, K]         (key: K)                      : ID[T] = ID1[T, K](key)
  final def apply[T <: DTO : ClassTag, K1, K2]    (key1: K1, key2: K2)          : ID[T] = ID2[T, K1, K2](key1, key2)
  final def apply[T <: DTO : ClassTag, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[T] = ID3[T, K1, K2, K3](key1, key2, key3)
}

final case class Instance[T <: DTO](value: T) extends Ref[T]
