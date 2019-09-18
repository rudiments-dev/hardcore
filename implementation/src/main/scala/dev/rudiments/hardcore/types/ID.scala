package dev.rudiments.hardcore.types

import dev.rudiments.hardcore.DTO

import scala.reflect.ClassTag

sealed trait Ref[T <: DTO] {}

sealed trait ID[T <: DTO] extends Ref[T] {}
object ID {
  final case class ID0[T: ClassTag]() extends ID[T]
  final case class ID1[T: ClassTag, K](key: K) extends ID[T]
  final case class ID2[T: ClassTag, K1, K2](key1: K1, key2: K2) extends ID[T]
  final case class ID3[T: ClassTag, K1, K2, K3](key1: K1, key2: K2, key3: K3) extends ID[T]

  def apply[T: ClassTag]: ID[T] = ID0[T]
  def apply[T: ClassTag, K](key: K): ID[T] = ID1[T, K](key)
  def apply[T: ClassTag, K1, K2](key1: K1, key2: K2): ID[T] = ID2[T, K1, K2](key1, key2)
  def apply[T: ClassTag, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[T] = ID3[T, K1, K2, K3](key1, key2, key3)
}

final case class Instance[T <: DTO](value: T) extends Ref[T]
