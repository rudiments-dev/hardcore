package dev.rudiments.hardcore

package object data {
  trait DataCommand[T] extends Command
  trait DataEvent[T] extends Event
  trait DataErrorEvent[T] extends DataEvent[T] with Error

  type DataSkill[T] = Skill[DataCommand[T], DataEvent[T]]
}
