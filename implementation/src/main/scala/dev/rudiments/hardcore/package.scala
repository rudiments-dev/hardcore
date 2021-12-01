package dev.rudiments

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

package object hardcore {
  implicit final class DataOps[A : ClassTag : TypeTag](private val value: A) {
    def asData: Data = Data.apply[A](value)
  }
}
