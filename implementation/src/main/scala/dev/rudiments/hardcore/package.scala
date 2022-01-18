package dev.rudiments

import dev.rudiments.hardcore.Size.Big

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

package object hardcore {
  implicit final class DataOps[A : ClassTag : TypeTag](private val value: A)(implicit space: Space) {
    def asData: Data = Data.apply[A](value)
  }

  implicit def int2Size: Int => Size = i => Big(BigDecimal(i))
}
