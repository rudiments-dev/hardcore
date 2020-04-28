package dev.rudiments.hardcore.http

import java.sql.{Date, Timestamp}

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import enumeratum._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.extras.{AutoDerivation, Configuration}
import io.circe.syntax._

import scala.language.implicitConversions

object CirceSupport extends AutoDerivation with FailFastCirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

  implicit val timestampFormat: Encoder[Timestamp] with Decoder[Timestamp] = new Encoder[Timestamp] with Decoder[Timestamp] {
    override def apply(a: Timestamp): Json = Encoder.encodeString.apply(a.toString)
    override def apply(c: HCursor): Result[Timestamp] = Decoder.decodeString.map(Timestamp.valueOf).apply(c)
  }

  implicit val dateFormat: Encoder[Date] with Decoder[Date] = new Encoder[Date] with Decoder[Date] {
    override def apply(a: Date): Json = Encoder.encodeString.apply(a.toString)
    override def apply(c: HCursor): Result[Date] = Decoder.decodeString.map(Date.valueOf).apply(c)
  }

  implicit def enumFormat[E <: EnumEntry](enum: Enum[E]): Encoder[E] with Decoder[E] = new Encoder[E] with Decoder[E] {
    override def apply(a: E): Json = a.entryName.asJson

    override def apply(c: HCursor): Result[E] = implicitly[Decoder[String]].apply(c).flatMap { s =>
      enum.withNameOption(s) match {
        case Some(member) => Right(member)
        case _ => Left(DecodingFailure(s"'$s' is not a member of enum $enum", c.history))
      }
    }
  }
}
