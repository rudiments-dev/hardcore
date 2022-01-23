package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRORouter}
import io.circe.Encoder

class Types(implicit space: Space) extends LazyLogging {
  implicit val encoder: Encoder[Thing] = CirceSupport.encode

  private val typesAgent = ID("types").asPath.find
  val router = new ScalaRORouter[Thing](
    Path(ID("types")),
    ScalaTypes.ScalaString,
    typesAgent
  )
}
