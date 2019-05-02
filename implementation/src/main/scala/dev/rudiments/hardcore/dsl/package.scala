package dev.rudiments.hardcore

package object dsl {
  type Handler = PartialFunction[Command, Event]
}
