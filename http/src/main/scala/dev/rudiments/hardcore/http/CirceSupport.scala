package dev.rudiments.hardcore.http

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore._
import io.circe._
import io.circe.generic.extras.Configuration

trait CirceSupport extends FailFastCirceSupport {
  implicit val configuration: Configuration = Configuration.default.withDefaults
  implicit val printer: Printer = Printer.noSpaces.copy(dropNullValues = true)

  implicit val dataEncoder: Encoder[Data] = CirceSupport.encodeData
  implicit def structuredEncoder[T](implicit en: Encoder[T]): Encoder[Node[T]] = CirceSupport.encodeNode
}

object CirceSupport {
  private def encodeData(data: Data): Json = encode(data.what, data.data)
  private def encodeNode[T](node: Node[T])(implicit en: Encoder[T]): Json = Json.obj(
    node.leafs.toSeq.map{ case (k, v) => k.toString -> en(v) } ++
      node.branches.toSeq.map { case (k, v) => k.toString -> encodeNode(v) } :_*
  )

  private def encode(p: Predicate, v: Any): Json = (p, v) match {
    case (t: Type, values: Seq[Any]) =>
      Json.obj(t.fields.zip(values).map { case (f, v) => (f.name, encode(f.of, v)) }:_*)
    case (p: Plain, v: Any) => encodePlain(p, v)
    case (Enlist(p), vs: Seq[Any]) => Json.arr(vs.map(v => encode(p, v)):_*)
    case (Index(_, pv), vs: Map[Location, Any]) => Json.obj(
      vs.toSeq.map { case (k, v) =>
        k.toString -> encode(pv, v)
      } :_*
    )
    case (a: AnyOf, v: Link) if a.p.contains(v) => Json.fromString(v.where.toString)
    case (other, another) =>
      Json.fromString(s"NOT IMPLEMENTED: $another")
  }

  private def encodePlain(p: Plain, v: Any): Json = (p, v) match {
    case (Text(_), s: String) => Json.fromString(s)
    case (Number(_, _), i: Int) => Json.fromInt(i)
    case (Number(_, _), l: Long) => Json.fromLong(l)
    case (Bool, b: Boolean) => Json.fromBoolean(b)
    case (Binary, Nothing) => Json.fromString("∅")
    case (_, None) => Json.Null
    case (_, _) => throw new IllegalArgumentException(s"Can't encode [$v] of $p ")
  }
}
