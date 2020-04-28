package dev.rudiments.hardcore.types

import scala.language.implicitConversions

sealed trait Hard[T] {}

sealed trait HardRef[T] extends Hard[T] {}

sealed trait HardID[T] extends HardRef[T] with ID {}
object       HardID {
  final case class HardAutoID [T]             ()                              extends HardID[T] with AutoID
  final case class HardID0    [T]             ()                              extends HardID[T]
  final case class HardID1    [T, K]          (key: K)                        extends HardID[T]
  final case class HardID2    [T, K1, K2]     (key1: K1, key2: K2)            extends HardID[T]
  final case class HardID3    [T, K1, K2, K3] (key1: K1, key2: K2, key3: K3)  extends HardID[T]

  final def auto [T]            ()                            : HardID[T] = HardAutoID[T]()
  final def apply[T]            ()                            : HardID[T] = HardID0[T]()
  final def apply[T, K]         (key: K)                      : HardID[T] = HardID1[T, K](key)
  final def apply[T, K1, K2]    (key1: K1, key2: K2)          : HardID[T] = HardID2[T, K1, K2](key1, key2)
  final def apply[T, K1, K2, K3](key1: K1, key2: K2, key3: K3): HardID[T] = HardID3[T, K1, K2, K3](key1, key2, key3)
}

final case class HardInstance[T](value: T) extends HardRef[T] with Instance
object           HardInstance {
  implicit def toInstance[T](value: T): HardInstance[T] = {
    HardInstance(value)
  }

  implicit def fromInstance[T](instance: HardInstance[T]): T = {
    instance.value
  }
}
