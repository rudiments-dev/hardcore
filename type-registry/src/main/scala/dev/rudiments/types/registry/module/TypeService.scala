package dev.rudiments.types.registry.module

import dev.rudiments.hardcore.data
import dev.rudiments.hardcore.data.DataSkill
import dev.rudiments.hardcore.types.Type

class TypeService(val f: DataSkill[Type]) extends DataSkill[Type] {
  override def isDefinedAt(x: data.DataCommand[Type]): Boolean = f.isDefinedAt(x)

  override def apply(v1: data.DataCommand[Type]): data.DataEvent[Type] = f.apply(v1)

}
