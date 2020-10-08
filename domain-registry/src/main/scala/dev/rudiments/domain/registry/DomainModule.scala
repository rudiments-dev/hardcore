package dev.rudiments.domain.registry

import dev.rudiments.data.DataHttpPort
import dev.rudiments.domain.{ID, ScalaTypes, SomeThing, Spec}
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import dev.rudiments.memory.Memory

class DomainModule {
  val ctx = new DomainContext
  val skill = new DomainSkill(ctx)
  val memory = new Memory(skill)

  def apply(name: String): Spec = ctx.domain.find[Spec](name)

  val http = new DataHttpPort(
    "domain",
    ScalaTypes.ScalaString,
    i => ID(Seq(i.extract[String]("name"))),
    memory
  )(
    ctx.domain.makeFromScala[Spec, SomeThing],
    new ThingEncoder(ctx.domain).abstractInstanceEncoder("Thing"),
    new ThingDecoder(ctx.domain).abstractInstanceDecoder("Thing")
  )
}
