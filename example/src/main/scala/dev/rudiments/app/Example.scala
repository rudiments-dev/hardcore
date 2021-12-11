package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ScalaRouter
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

class Example extends LazyLogging {
  implicit val en: Encoder[Body] = deriveEncoder[Body]
  implicit val de: Decoder[Body] = deriveDecoder[Body]
  val exampleAgent = new Memory(Type.build[In], Type.build[Out])
  val router = new ScalaRouter[Body](
    Path(ID("example")),
    ScalaTypes.ScalaString,
    exampleAgent
  )
}
