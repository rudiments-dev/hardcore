package dev.rudiments.types.registry.module

import dev.rudiments.hardcore.data.{DataHttpPort, DataSkill}
import dev.rudiments.hardcore.types.{HardID, Type}

object TypeHttpPort {

  import dev.rudiments.hardcore.http.CirceSupport._
  def apply(prefix: String, f: DataSkill[Type]): DataHttpPort[Type, String] = new DataHttpPort[Type, String](
    prefix,
    e => HardID(e.name),
    f
  )
}