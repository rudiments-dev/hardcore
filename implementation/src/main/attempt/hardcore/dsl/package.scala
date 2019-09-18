package dev.rudiments.hardcore

package object dsl {
  //TODO make this types parametrized
  type PF1 = PartialFunction[Command, Event]

  type PF2 = PartialFunction[(Command, Event), Event]
  type Resolver = PartialFunction[Command, Command]
}
