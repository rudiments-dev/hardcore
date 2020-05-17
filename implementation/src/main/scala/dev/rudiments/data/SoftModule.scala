package dev.rudiments.data

import dev.rudiments.hardcore.flow.{ControlFlow, Controlled}
import dev.rudiments.hardcore.{Skill, asSkill}
import dev.rudiments.hardcore.http.{Router, SoftDecoder, SoftEncoder}
import dev.rudiments.hardcore.types.{ID, Instance, SoftID, SoftInstance, Type}
import io.circe.{Decoder, Encoder}

class SoftModule (
  val context: ModuleContext,
  custom: Seq[(String, Router)] = Seq.empty,
  customId: Seq[(String, ID => Router)] = Seq.empty
) {
  val port = new DataHttpPort(
    context.prefix,
    context.idField,
    context.id,
    context.adapter,
    custom,
    customId
  )(context.t, context.encoder, context.decoder)
}

object SoftModule {
  def apply(
    prefix: String,
    idField: String,
    custom: Seq[(String, ModuleContext => Router)] = Seq.empty,
    customId: Seq[(String, ModuleContext => ID => Router)] = Seq.empty
  )(implicit t: Type): SoftModule = {

    implicit val flow: ControlFlow = new ControlFlow()

    val context = ModuleContext(
      t,
      idField,
      flow,
      new Controlled(asSkill(new SoftCache()(t))),
      prefix,
      SoftEncoder(t).contramap { case i: SoftInstance => i },
      SoftDecoder(t).map(_.asInstanceOf[Instance])
    )
    new SoftModule(
      context,
      custom.map { case (s, f) => (s, f(context))},
      customId.map { case (s, f) => (s, f(context))}
    )
  }
}

case class ModuleContext(
  t: Type,
  idField: String,
  flow: ControlFlow,
  adapter: Skill,
  prefix: String,
  encoder: Encoder[Instance],
  decoder: Decoder[Instance]
) {
  val id: Instance => ID = {
    case i: SoftInstance => SoftID(t.extract(i, idField))(t)
    case other => ???
  }
}
