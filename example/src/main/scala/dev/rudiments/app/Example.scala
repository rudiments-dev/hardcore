package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, Encoder}

class Example(implicit space: Space) extends LazyLogging {
  val de: Decoder[Body] = deriveDecoder[Body]

  implicit val encoder: Encoder[Thing] = CirceSupport.encode
  implicit val decoder: Decoder[Thing] = de.map(_.asData)

  private val exampleAgent = new Memory(All, All)
  val router = new ScalaRouter[Body](
    Path(ID("example")),
    ScalaTypes.ScalaString,
    exampleAgent
  )
}
