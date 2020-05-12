package dev.rudiments.data

import dev.rudiments.hardcore.flow.{ControlFlow, Controlled}
import dev.rudiments.hardcore.asSkill
import dev.rudiments.hardcore.types.{ID, Instance, SoftID, SoftInstance, Type}

class SoftModule (
  prefix: String,
  idField: String
)(implicit t: Type) {

  private val identify: Instance => ID = {
    case i: SoftInstance => SoftID(t.extract(i, idField))
    case other => ???
  }

  implicit val flow = new ControlFlow
  val cache = new Controlled(asSkill(new SoftCache()))
  val port = new DataHttpPort(prefix, idField, identify, cache)
}
