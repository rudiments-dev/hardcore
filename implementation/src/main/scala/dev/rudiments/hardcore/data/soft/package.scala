package dev.rudiments.hardcore.data

import dev.rudiments.hardcore._

package object soft {
  trait DataCommand extends Command
  trait DataEvent extends Event
  trait DataErrorEvent extends DataEvent with Error

  type DataSkill = Skill[DataCommand, DataEvent]
}
