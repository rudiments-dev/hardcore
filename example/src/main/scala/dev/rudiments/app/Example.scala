package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ScalaRouter
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

class Example(implicit space: Space) extends LazyLogging {
  implicit val decoder: Decoder[Thing] = deriveDecoder[Body].map(_.asData)
  val router = new ScalaRouter(
    ScalaTypes.ScalaString,
    new Memory(All, All)
  )
}
