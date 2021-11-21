package dev.rudiments

import scala.reflect.ClassTag

package object hardcore {
  implicit final class DataOps[A : ClassTag](private val value: A) {
    def asData: Data = Data.apply[A](value)
  }
}
