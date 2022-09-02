package dev.rudiments.hardcore.http

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore._
import io.circe._
import io.circe.generic.extras.Configuration

trait CirceSupport extends FailFastCirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

  implicit val thingEncoder: Encoder[Thing] = ThingEncoder.encodeAnything
  implicit val dataEncoder: Encoder[Data] = ThingEncoder.encodeAnything
  implicit def memEncoder: Encoder[Node] = ThingEncoder.encodeNode
  implicit val outEncoder: Encoder[CRUD.O] = ThingEncoder.encodeOut
}
