package dev.rudiments.data

import dev.rudiments.hardcore.Skill
import dev.rudiments.hardcore.http.{Router, ThingDecoder, ThingEncoder}
import dev.rudiments.domain.{Domain, ID, Instance, Spec}
import io.circe.{Decoder, Encoder}

class SoftModule (
  val context: ModuleContext,
  custom: Seq[(String, Router)] = Seq.empty,
  customId: Seq[(String, ID => Router)] = Seq.empty
) {
  val port = new DataHttpPort(
    context.prefix,
    context.spec.fields(context.idField).thing,
    context.id,
    context.adapter,
    custom,
    customId
  )(context.spec, context.encoder, context.decoder)
}

object SoftModule {
  def apply(
    prefix: String,
    idField: String,
    custom: Seq[(String, ModuleContext => Router)] = Seq.empty,
    customId: Seq[(String, ModuleContext => ID => Router)] = Seq.empty
  )(implicit spec: Spec, domain: Domain): SoftModule = {

    val context = ModuleContext(
      spec,
      idField,
      new State,
      prefix,
      new ThingEncoder(domain).specEncoder(spec),
      new ThingDecoder(domain).specDecoder(spec)
    )
    new SoftModule(
      context,
      custom.map { case (s, f) => (s, f(context))},
      customId.map { case (s, f) => (s, f(context))}
    )
  }
}

case class ModuleContext(
  spec: Spec,
  idField: String,
  adapter: Skill,
  prefix: String,
  encoder: Encoder[Instance],
  decoder: Decoder[Instance]
) {
  val id: Instance => ID = it => ID(Seq(it.extract[Any](idField)))
}
