package work.unformed.hardcore.http

import java.sql.{Date, Timestamp}

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.generic.extras.{AutoDerivation, Configuration}
import io.circe._
import work.unformed.hardcore.dsl.SortOrder.{Asc, Desc}
import work.unformed.hardcore.dsl._

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

  implicit def filterEncoder[A]: Encoder[Filter[A]] = new Encoder[Filter[A]] {
    override def apply(a: Filter[A]): Json = Encoder.encodeString.apply(a.toString)
  }

  implicit def sortEncoder[A]: Encoder[Sort[A]] = new Encoder[Sort[A]] {
    override def apply(a: Sort[A]): Json = Encoder.encodeString.apply(a.order match {
      case Desc => "!" + a.field
      case Asc => a.field
    })
  }

  implicit def instance[A: Encoder]: Encoder[Instance[A]] =
    implicitly[Encoder[A]].contramap(_.value)
}
