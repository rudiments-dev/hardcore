package dev.rudiments.domain.registry

import dev.rudiments.data.DataHttpPort
import dev.rudiments.domain.{Domain, ID, Instance, ScalaTypes, Spec}
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import dev.rudiments.memory.Memory
import io.circe.{Decoder, Encoder}

class DomainModule {
  val skill: Domain = new Domain
  val memory = new Memory(skill)

  def apply(name: String): Spec = skill.find[Spec](name)

  val encoder: Encoder[Instance] = new ThingEncoder(skill).specEncoder("Type")
  val decoder: Decoder[Instance] = new ThingDecoder(skill).specDecoder("Type")

  val http = new DataHttpPort(
    "domain",
    ScalaTypes.ScalaString,
    i => ID(Seq(i.extract[String]("name"))),
    memory
  )(
    skill.find[Spec]("Type"),
    encoder,
    decoder
  )
}
