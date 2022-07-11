package dev.rudiments.hardcore.http

import dev.rudiments.hardcore._
import io.circe.{Encoder, Json}

object ThingEncoder {
  def encodeData(data: Data): Json = encode(data.what, data.data)

  def encodeNode[T](node: Node[T])(implicit en: Encoder[T]): Json = {
    val leafs = node.leafs.toSeq.map{ case (k, v) => k.toString -> en(v) }
    val branches = node.branches.toSeq.map { case (k, v) => k.toString -> encodeNode(v) }
    val all = node.self
      .map { s => leafs ++ branches :+ ("self" -> en(s)) }
      .getOrElse(leafs ++ branches)
    Json.obj(all :_*)
  }

  def encodeNodeRaw[T](node: Node[T])(f: T => Json): Json = {
    val leafs = node.leafs.toSeq.map{ case (k, v) => k.toString -> f(v) }
    val branches = node.branches.toSeq.map { case (k, v) => k.toString -> encodeNodeRaw(v)(f) }
    val all = node.self
      .map { s => leafs ++ branches :+ ("self" -> f(s)) }
      .getOrElse(leafs ++ branches)
    Json.obj(all :_*)
  }

  def encode(p: Predicate, v: Any): Json = (p, v) match {
    case (l: Link, values) => encode(l.what, values) //TODO add 'type' from Link's location
    case (t: Type, values: Seq[Any]) =>
      Json.obj(t.fields.zip(values).map { case (f, v) => (f.name, encode(f.of, v)) }:_*)
    case (p: Plain, v: Any) => encodePlain(p, v)
    case (Enlist(p), vs: Seq[Any]) => Json.arr(vs.map(v => encode(p, v)):_*)
    case (Index(_, pv), vs: Map[Location, Any]) => Json.obj(
      vs.toSeq.map { case (k, v) => k.toString -> encode(pv, v) } :_*
    )
    case (a: AnyOf, v: Link) if a.p.contains(v) => Json.fromString(v.where.toString)
    case (other, another) =>
      Json.fromString(s"NOT IMPLEMENTED: $another")
  }

  def encodePlain(p: Plain, v: Any): Json = (p, v) match {
    case (Text(_), s: String) => Json.fromString(s)
    case (Number(_, _), i: Int) => Json.fromInt(i)
    case (Number(_, _), l: Long) => Json.fromLong(l)
    case (Bool, b: Boolean) => Json.fromBoolean(b)
    case (Binary, Nothing) => Json.fromString("∅")
    case (_, None) => Json.Null
    case (_, _) => throw new IllegalArgumentException(s"Can't encode [$v] of $p ")
  }

  def encodeOut(out: Memory.O): Json = out match {
    case evt: Memory.Evt => encodeEvent(evt)
    case Readen(Data(p, v)) => Json.obj(
      "type" -> Json.fromString("?"),
      "data" -> encode(p, v)
    )
    case NotExist => Json.fromString("NotExist")
    case Prepared(cmt) => Json.obj(
      "type" -> Json.fromString("Prepared"),
      "CRUD" -> encodeNodeRaw(Node.fromMap(cmt.crud))(encodeEvent)
    )
    case other => Json.fromString(s"NOT IMPLEMENTED: $other")
  }

  def encodeEvent(evt: Memory.Evt): Json = evt match {
    case Created(Data(p, v)) => Json.obj(
      "type" -> Json.fromString("+"),
      "data" -> encode(p, v)
    )
    case Updated(o, Data(p, v)) => Json.obj(
      "type" -> Json.fromString("*"),
      "data" -> encode(p, v),
      "old" -> encode(o.what, o.data)
    )
    case Deleted(Data(p, v)) => Json.obj(
      "type" -> Json.fromString("-"),
      "old" -> encode(p, v)
    )
    case Committed(cmt) => Json.obj(
      "type" -> Json.fromString("Committed"),
      "CRUD" -> encodeNodeRaw(Node.fromMap(cmt.crud))(encodeEvent)
    )
  }
}
