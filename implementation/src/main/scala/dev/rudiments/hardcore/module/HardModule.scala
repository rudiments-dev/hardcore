package dev.rudiments.hardcore.module

import dev.rudiments.hardcore.data.{DataHttpPort, DataMemoryAdapter}
import dev.rudiments.hardcore.types.{DTO, HardType, ID}
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag

class HardModule[T <: DTO : TypeTag : Encoder : Decoder, K : TypeTag](prefix: String, identify: T => ID[T]) {
  implicit val t: HardType[T] = HardType[T]
  val adapter: DataMemoryAdapter[T] = new DataMemoryAdapter()
  val port: DataHttpPort[T, K] = new DataHttpPort[T, K](prefix, identify, adapter)
}
