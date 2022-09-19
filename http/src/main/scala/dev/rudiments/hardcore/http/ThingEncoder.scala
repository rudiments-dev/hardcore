package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import io.circe.{Json, KeyEncoder}

import java.sql

object ThingEncoder {
  val codecs: ID = ID("codecs")
  val jsonCodec: Location = codecs / "json"

  def init(ctx: Node): Commit = {
    val tx = new Tx(ctx)
    tx += codecs -> Node.empty
    tx += jsonCodec -> Node(leafIs = Internal)

    val foundTypes = ctx ??* types match {
      case Found(_, values) => values
      case other => throw new IllegalStateException(s"Can't read /types, got $other")
    }

    foundTypes

    val prepared = tx.>>
    prepared match {
      case Prepared(c) => ctx << c match {
        case Committed(cmt) =>
          cmt
        case _ => throw new IllegalStateException("Json Encoder commit failed")
      }
      case _ => throw new IllegalStateException("Json Encoder  commit not prepared")
    }
  }

  val discriminator = "type"
  val partners: Location = ID("Partners")

  implicit val idEncoder: KeyEncoder[ID] = KeyEncoder.encodeKeyString.contramap(id => id.key.toString)
  implicit val pathEncoder: KeyEncoder[Path] = KeyEncoder.encodeKeyString.contramap(path => path.toString)

  def encodeData(data: Data): Json = encode(data.what, data.data)

  def encodeNode(node: Node): Json = {
    val selfJson = Seq(
      "self" -> node.self,
      "key-is" -> node.keyIs,
      "leaf-is" -> node.leafIs,
    ).collect {
      case (s, v) if v != Nothing => s -> encodeAnything(v)
    }

    val leafs = node.leafs.toSeq.map { case (k, v) => idEncoder(k) -> encodeAnything(v) }
    val branches = node.branches.toSeq.map { case (k, v) => idEncoder(k) -> encodeNode(v) }

    val leafJson = if(leafs.nonEmpty) {
      Seq("leafs" -> Json.obj(leafs: _*))
    } else {
      Seq.empty
    }

    val branchesJson = if(branches.nonEmpty) {
      Seq("branches" -> Json.obj(branches: _*))
    } else {
      Seq.empty
    }

    val all = Seq("type" -> Json.fromString("Node")) ++ selfJson ++ leafJson ++ branchesJson

    Json.obj(all: _*)
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
    case (Link(_, any: AnyOf), l: Link) =>
      val found = any.p.collect {
        case f: Link if f == l => f
      }
      if(found.size == 1) {
        Json.fromString(found.head.where.lastString)
      } else {
        throw new IllegalArgumentException(s"Linked $l link not in AnyOf")
      }
    case (loc: Link, l: Location) if loc.where == types / "Location" =>
      Json.fromString(l.toString)
    case (l: Link, values) =>
      encode(l.what, values) //TODO add 'type' from Link's location
    case (t: Type, values: Seq[Any]) =>
      Json.obj(t.fields.zip(values).map { case (f, v) => (f.name, encode(f.of, v)) }:_*)
    case (p: Plain, v: Any) => encodePlain(p, v)
    case (Enlist(p), vs: Seq[Any]) =>
      Json.arr(vs.map(v => encode(p, v)):_*)
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
    case (Binary, _) => Json.fromString("--BINARY--")
    case (Date, d: sql.Date) => Json.fromString(d.toString)
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
        "up-to" -> Json.fromString(upTo.toString)
      )
      case Text(maxSize) => Json.obj(
        "type" -> Json.fromString("Text"),
        "max-size" -> Json.fromString(maxSize.toString),
      )
      case Bool => Json.obj(discriminator -> Json.fromString("Bool"))
      case Binary => Json.obj(discriminator -> Json.fromString("Binary"))
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
