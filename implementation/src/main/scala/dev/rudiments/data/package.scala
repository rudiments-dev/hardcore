package dev.rudiments

import dev.rudiments.hardcore._

package object data {
  trait DataCommand extends Command
  trait DataEvent extends Event
  trait DataErrorEvent extends DataEvent with Error

  type DataSkill = HardSkill[DataCommand, DataEvent]
}
