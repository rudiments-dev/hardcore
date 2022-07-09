package dev.rudiments.hardcore.http

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore._
import io.circe._
import io.circe.generic.extras.Configuration

trait CirceSupport extends FailFastCirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

  implicit val dataEncoder: Encoder[Data] = ThingEncoder.encodeData
  implicit def structuredEncoder[T](implicit en: Encoder[T]): Encoder[Node[T]] = ThingEncoder.encodeNode
}
