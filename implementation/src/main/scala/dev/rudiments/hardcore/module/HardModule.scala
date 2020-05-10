package dev.rudiments.hardcore.module

import dev.rudiments.hardcore.data.{DataHttpPort, HardCache}
import dev.rudiments.hardcore.types.HardID
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag

class HardModule[T : Encoder : Decoder, K : TypeTag](prefix: String, identify: T => HardID[T]) {
  val cache: HardCache[T] = new HardCache()
  val port: DataHttpPort[T, K] = new DataHttpPort[T, K](prefix, identify, cache)
}
