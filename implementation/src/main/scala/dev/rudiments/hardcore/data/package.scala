package dev.rudiments.hardcore

import dev.rudiments.hardcore.types.{DTO, ID, HardType}

package object data {
  trait DataCommand [T <: DTO] extends Command
  trait DataEvent[T <: DTO] extends Event
  trait DataErrorEvent[T <: DTO] extends DataEvent[T] with Error

  type DataSkill[T <: DTO] = Skill[DataCommand[T], DataEvent[T]]
}
