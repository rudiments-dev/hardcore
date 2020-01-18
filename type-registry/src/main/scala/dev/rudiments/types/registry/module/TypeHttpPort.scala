package dev.rudiments.types.registry.module

import dev.rudiments.hardcore.data.{DataHttpPort, DataSkill}
import dev.rudiments.hardcore.types.{HardType, ID, Type}

object TypeHttpPort {
  private implicit val t: HardType[Type] = HardType[Type]

  import dev.rudiments.hardcore.http.CirceSupport._
  def apply(prefix: String, f: DataSkill[Type]): DataHttpPort[Type, String] = new DataHttpPort[Type, String](
    prefix,
    e => ID(e.name),
    f
  )
}