package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ScalaRouter
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

class Example(implicit space: Space) extends LazyLogging {
  val en: Encoder[Body] = deriveEncoder[Body]
  val de: Decoder[Body] = deriveDecoder[Body]

  implicit val encoder: Encoder[Thing] = en.contramap { case d: Data => d.reconstruct[Body]() }
  implicit val decoder: Decoder[Thing] = de.map(_.asData)

  private val exampleAgent = new Memory(All, All)
  val router = new ScalaRouter[Body](
    Path(ID("example")),
    ScalaTypes.ScalaString,
    exampleAgent
  )
}
