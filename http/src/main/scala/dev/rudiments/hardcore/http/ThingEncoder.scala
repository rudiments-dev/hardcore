package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.Predicate.Anything
import dev.rudiments.hardcore._
import io.circe.{Encoder, Json, KeyEncoder}

object ThingEncoder {
  val discriminator = "type"
  val partners: Location = ID("types") / "Partners"

  implicit val idEncoder: KeyEncoder[ID] = KeyEncoder.encodeKeyString.contramap(id => id.key.toString)
  implicit val pathEncoder: KeyEncoder[Path] = KeyEncoder.encodeKeyString.contramap(path => path.toString)

  def encodeData(data: Data): Json = encode(data.what, data.data)

  def encodeNode(node: Node): Json = {
    if(node.leafs.isEmpty && node.branches.isEmpty) {
      if(node.relations.contains(partners)) {
        Json.obj(
          "type" -> Json.fromString("Node"),
          "values" -> Json.arr(
            node.relations(partners).map(v => Json.fromString(v.lastString)):_*
          )
        )
      } else {
        Json.obj(
          "type" -> Json.fromString("Node"),
          "self" -> encodeAnything(node.self),
          "leaf-is" -> encodePredicate(node.leafIs),
          "key-is" -> encodePredicate(node.keyIs)
        )
      }
    } else {
      val leafs = node.leafs.toSeq.map { case (k, v) => k -> encodeAnything(v) }
      val branches = node.branches.toSeq.map { case (k, v) => k -> encodeNode(v) }
      val all: Seq[(ID, Json)] = if (node.self != Nothing) {
        leafs ++ branches :+ (ID("self") -> encodeAnything(node.self))
      } else {
        leafs ++ branches
      }
      val keyEncoded = all.map { case (id, j) => idEncoder(id) -> j }

      Json.obj(keyEncoded: _*)
    }
  }

  def encodeAnything(thing: Thing): Json = thing match {
    case Data(p, v) => encode(p, v)
    case o: CRUD.O => encodeOut(o)
    case p: Predicate => encodePredicate(p)
    case c: Commit => Json.obj(discriminator -> Json.fromString("Commit"), "crud" -> encodeNode(c.crudNode()))
    case n: Node => encodeNode(n)
    case other =>
      Json.fromString(s"NOT IMPLEMENTED something: $other")
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
    case (Binary, _) => Json.fromString("TBD")
    case (_, None) => Json.Null
    case (_, _) => throw new IllegalArgumentException(s"Can't encode [$v] of $p ")
  }

  def encodeOut(out: CRUD.O): Json = out match {
    case evt: CRUD.Evt => encodeEvent(evt)
    case Readen(Data(Link(l, p), v)) =>
      Json.obj(
        discriminator -> Json.fromString(l.toString),
        "thing" -> encode(p, v)
      )
    case Readen(Link(l, p)) =>
      p match {
        case Anything | Nothing => Json.obj(
          discriminator -> Json.fromString(l.toString)
        )
        case other => Json.obj(
          discriminator -> Json.fromString(l.toString),
          "thing" -> encodePredicate(other)
        )
      }
    case Readen(p: Predicate) => encodePredicate(p)
    case Readen(t) => Json.obj(
      discriminator -> Json.fromString("?"),
      "thing" -> encodeAnything(t)
    )
    case NotExist => Json.fromString("NotExist")
    case NotImplemented => Json.obj("type" -> Json.fromString("NotImplemented"))
    case Prepared(cmt) => Json.obj(
      discriminator -> Json.fromString("Prepared"),
      "CRUD" -> encodeNode(cmt.crudNode())
    )
    case other =>
      Json.fromString(s"NOT IMPLEMENTED Out: $other")
  }

  def encodeEvent(evt: CRUD.Evt): Json = evt match {
    case Created(t) => Json.obj(
      discriminator -> Json.fromString("Created"),
      "data" -> encodeAnything(t)
    )
    case Updated(o, n) => Json.obj(
      discriminator -> Json.fromString("Updated"),
      "new" -> encodeAnything(n),
      "old" -> encodeAnything(o)
    )
    case Deleted(d) => Json.obj(
      discriminator -> Json.fromString("Deleted"),
      "old" -> encodeAnything(d)
    )
    case Committed(cmt) => Json.obj(
      discriminator -> Json.fromString("Committed"),
      "CRUD" -> encodeNode(cmt.crudNode())
    )
  }

  def encodePredicate(p: Predicate): Json = p match {
    case Anything => Json.obj(discriminator -> Json.fromString("Anything"))
    case Nothing => Json.obj(discriminator -> Json.fromString("Nothing"))
    case p: Plain => p match {
      case Number(from, upTo) => Json.obj(
        "type" -> Json.fromString("Number"),
        "from" -> Json.fromString(from.toString),
        "upTo" -> Json.fromString(upTo.toString)
      )
      case Text(maxSize) => Json.obj(
        "type" -> Json.fromString("Text"),
        "maxSize" -> Json.fromString(maxSize.toString),
      )
      case Bool => Json.obj("type" -> Json.fromString("Bool"))
      case Binary => Json.obj("type" -> Json.fromString("Binary"))
      case other => Json.fromString(s"OTHER PREDICATE: $other")
    }
    case t: Type => Json.obj((discriminator -> Json.fromString("Type")) +: t.fields.map{ f => f.name -> encodePredicate(f.of) } :_*)
    case Declared(l) => Json.obj(discriminator -> Json.fromString(l.lastString))
    case Enlist(p) => Json.obj(discriminator -> Json.fromString("Enlist"), "of" -> encodePredicate(p))
    case Index(k, v) => Json.obj(discriminator -> Json.fromString("Index"), "of" -> encodePredicate(k), "over" -> encodePredicate(v))
    case Link(l, p) => p match {
      case Anything | Nothing | Bool => Json.fromString(l.lastString)
      case _: Type => Json.obj(discriminator -> Json.fromString(l.lastString))
      case _: Declared => Json.obj(discriminator -> Json.fromString(l.lastString))
      case _: AnyOf => Json.obj(discriminator -> Json.fromString(l.lastString))
      case other =>
        Json.fromString(s"NOT IMPLEMENTED Predicate: $other")
    }
    case a: AnyOf =>
      val links = a.p.collect { case l: Link => l.where }.toSeq
      if(a.p.size == links.size) { // AnyOf(Link*)
        Json.obj(discriminator -> Json.fromString("AnyOf"), "p" -> encodeEnum(links))
      } else {
        ???
      }
    case other =>
      Json.fromString(s"NOT IMPLEMENTED Predicate: $other")
  }

  def encodeEnum(values: Seq[Location]): Json = {
    Json.arr(values.map(v => Json.fromString(v.lastString)): _*)
  }
}
