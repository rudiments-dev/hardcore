package dev.rudiments.hardcore.http

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore._
import io.circe._
import io.circe.generic.extras.Configuration

trait CirceSupport extends FailFastCirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

  implicit val dataEncoder: Encoder[Data] = CirceSupport.encodeData
  implicit val eventEncoder: Encoder[Event] = dataEncoder.contramap {
    case Created(data) => data
    case Updated(_, data) => data
    case Deleted(data) => data
    case Committed(_) => ???
  }
}

object CirceSupport {
  private def encodeData(data: Data): Json = (data.what, data.data) match {
    case (t: Type, v: Seq[Any])  => encode(t, v)
    case (Binary, Nothing) => Json.Null
    case (Enlist(p), vs: Seq[Any]) => Json.arr(vs.map(v => encode(p, v)):_*)
    case (Index(_, pv), vs: Map[ID, Any]) => Json.obj(
      vs.toSeq.map { case (k, v) =>
        k.key.toString -> encode(pv, v)
      } :_*
    )
    case (other, another) =>
      ???
  }

  private def encode(p: Predicate, v: Any): Json = (p, v) match {
    case (t: Type, values: Seq[Any]) =>
      Json.obj(t.fields.zip(values).map { case (f, v) => (f.name, encode(f.of, v)) }:_*)
    case (p: Plain, v: Any) => encodePlain(p, v)
    case (other, another) =>
      Json.fromString(s"NOT IMPLEMENTED: $another")
  }

  private def encodePlain(p: Plain, v: Any): Json = (p, v) match {
    case (Text(_), s: String) => Json.fromString(s)
    case (Number(_, _), i: Int) => Json.fromInt(i)
    case (Number(_, _), l: Long) => Json.fromLong(l)
    case (Bool, b: Boolean) => Json.fromBoolean(b)
    case (_, None) => Json.Null
    case (_, _) => throw new IllegalArgumentException(s"Can't encode [$v] of $p ")
  }
}
