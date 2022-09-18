package dev.rudiments.hardcore.http

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder.discriminator
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

import java.sql
import scala.collection.{Factory, mutable}

class ThingDecoder(ts: TypeSystem) {
  def locKeyDecoder: KeyDecoder[Location] = KeyDecoder { s =>
    Location(s) match {
      case Root => Some(Root)
      case id: ID => Some(id)
      case path: Path => Some(path)
      case _ => None
    }
  }
  def idKeyDecoder: KeyDecoder[ID] = KeyDecoder { k => Some(ID(k)) }

  def locDecoder: Decoder[Location] = Decoder { c =>
    c.downField("missing").as[String].flatMap { s =>
      Location(s) match {
        case Root => Right(Root)
        case id: ID => Right(id)
        case path: Path => Right(path)
        case _ => Left(DecodingFailure("Not a location", List.empty))
      }
    }
  }

  def anythingDecoder: Decoder[Thing] = { c: HCursor =>
    c.downField(discriminator).as[String].flatMap { s =>
      val id = ID(s)
      val d = if(ts.typeSystem.contains(id)) {
        decoders(id)
      } else {
        alwaysFail(s"Not in /types: $s")
      }
      d.apply(c)
    }
  }

  val locationDecoders: Map[ID, Decoder[Thing]] = Map(
    ID("ID") -> Decoder { c: HCursor =>
      c.downField("key").as[String].map(s => ID(s).asInstanceOf[Thing])
    },
    ID("Path") -> Decoder { c: HCursor =>
      c.downField("ids").as[String]
        .map(s => Path.apply(s.split("/").map(ID): _*).asInstanceOf[Thing])
    },
    ID("Root") -> staticDecoder(Root),
    ID("Unmatched") -> staticDecoder(Unmatched),
  )

  val plainDecoders: Map[ID, Decoder[Predicate]] = Map(
    ID("Number") -> Decoder {_ => Right(Number(Long.MinValue, Long.MaxValue)) },
    ID("Text") -> Decoder {_ => Right(Text(1024)) },
    ID("Bool") -> Decoder {_ => Right(Bool) },
    ID("Binary") -> Decoder {_ => Right(Binary) },

    ID("Date") -> Decoder {_ => Right(Date) },
    ID("Time") -> Decoder {_ => Right(Time) },
    ID("Timestamp") -> Decoder {_ => Right(Timestamp) },
  )

  def predicateDecoder: Decoder[Predicate] = Decoder { c: HCursor =>
    c.downField(discriminator).as[String].flatMap { s =>
      val id = ID(s)
      if(plainDecoders.contains(id)) {
        plainDecoders(id).apply(c)
      } else {
        if (ts.predicates.contains(id)) {
          id match {
            case ID("Type") => typeDecoder.map(_.asInstanceOf[Predicate]).apply(c)
            case ID("Enlist") => c.downField("of").as(predicateDecoder.map(p => Enlist(p)))
            case ID("Index") => for {
              of <- c.downField("of").as(predicateDecoder)
              over <- c.downField("over").as(predicateDecoder)
            } yield Index(of, over)
            case ID("AnyOf") => c.downField("p")
              .as(Decoder.decodeArray(predicateDecoder, Factory.arrayFactory))
              .map(arr => AnyOf(arr:_*))
            case ID("Link") => Left(DecodingFailure("Not supported", List.empty))
            case ID("Declared") => Left(DecodingFailure("Not supported", List.empty))
            case _ => Left(DecodingFailure(s"Not implemented Predicate: $id", List.empty))
          }
        } else {
          Left(DecodingFailure(s"Not a Predicate: $id", List.empty))
        }
      }
    }
  }

  def nodeDecoder: Decoder[Node] = Decoder { c =>
    for {
      self <- c.getOrElse("self")(Nothing.asInstanceOf[Thing])(anythingDecoder)
      keyIs <- c.getOrElse("keyIs")(Nothing.asInstanceOf[Predicate])(predicateDecoder)
      leafIs <- c.getOrElse("leafIs")(Nothing.asInstanceOf[Predicate])(predicateDecoder)
      leafs <- c.getOrElse("leafs")(Map.empty[ID, Thing])(
        Decoder.decodeMap(idKeyDecoder, anythingDecoder)
      ) //TODO propagate leafIs and keyIs predicates for decoding leafs
      branches <- c.getOrElse("branches")(Map.empty[ID, Node])(
        Decoder.decodeMap(idKeyDecoder, nodeDecoder)
      )
      relations <- c.getOrElse("relations")(Map.empty[Location, Seq[Location]])(
        Decoder.decodeMap(
          locKeyDecoder,
          Decoder
            .decodeArray(locDecoder, Factory.arrayFactory)
            .map(_.toSeq)))
    } yield new Node(
      self,
      mutable.Map.from(leafs),
      mutable.Map.from(branches),
      mutable.Map.from(relations),
      keyIs,
      leafIs,
    )
  }

  private def typeDecoder: Decoder[Type] = { c: HCursor =>
    c.keys.getOrElse(throw new IllegalStateException("How this happened?"))
      .collect { case k if k != discriminator => c.downField(k).as(predicateDecoder).map(p => Field(k, p)) }
      .foldRight(Right(scala.Nil): Decoder.Result[scala.List[Field]]) { (e, acc) =>
        for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map { l => Type(l: _*) }
  }

  val compositeDecoders: Map[ID, Decoder[Thing]] = Map(
    ID("Field") -> alwaysFail("Direct Field decoding not supported"),
    ID("Type") -> typeDecoder.map(_.asInstanceOf[Thing]),
    ID("Enlist") -> Decoder { c: HCursor => c.downField("of").as(predicateDecoder.map(p => Enlist(p).asInstanceOf[Thing])) },
    ID("Index") -> Decoder { c: HCursor =>
      for {
        of <- c.downField("of").as(predicateDecoder)
        over <- c.downField("over").as(predicateDecoder)
      } yield Index(of, over).asInstanceOf[Thing]
    },
    ID("AnyOf") -> Decoder { c: HCursor =>
      c.downField("p")
        .as(Decoder.decodeArray(predicateDecoder, Factory.arrayFactory))
        .map(arr => AnyOf(arr: _*))
    },
    ID("Link") -> alwaysFail("TODO Link"),
    ID("Declared") -> alwaysFail("TODO Declared"),
  )

  def commitDecoder: Decoder[Commit] = Decoder {_ => Left(DecodingFailure("TODO: Commit", List.empty)) }

  val messageDecoders: Map[ID, Decoder[Thing]] = Map(
    ID("Create") -> anythingDecoder.map(Create),
    ID("Read") -> staticDecoder(Read),
    ID("Update") -> anythingDecoder.map(Update),
    ID("Delete") -> staticDecoder(Delete),
    ID("Find") -> predicateDecoder.map(Find),
    ID("LookFor") -> predicateDecoder.map(LookFor),
    ID("Dump") -> predicateDecoder.map(Dump),
    ID("Prepare") -> staticDecoder(Prepare),
    ID("Verify") -> staticDecoder(Verify),
    ID("Commit") -> commitDecoder.map(_.asInstanceOf[Thing]),

    ID("Created") -> anythingDecoder.map(Created),
    ID("Readen") -> anythingDecoder.map(Readen),
    ID("Updated") -> Decoder { c: HCursor =>
      for {
        old <- c.downField("old").as(anythingDecoder)
        what <- c.downField("what").as(anythingDecoder)
      } yield Updated(old, what)
    },
    ID("Deleted") -> anythingDecoder.map(Deleted),
    ID("Found") -> Decoder { c: HCursor =>
      for {
        query <- c.downField("query").as(anythingDecoder).flatMap {
          case q: Query => Right(q)
          case other => Left(DecodingFailure("Not supported thing instead of Query", List.empty))
        }
        values <- c.downField("what").as(Decoder.decodeMap(locKeyDecoder, anythingDecoder))
      } yield Found(query, values)
    },

    ID("NotExist") -> staticDecoder(NotExist),
    ID("NotFound") -> Decoder { _.downField("missing").as(locDecoder).map(NotFound) },
    ID("Prepared") -> commitDecoder.map(cmt => Prepared(cmt).asInstanceOf[Thing]),
    ID("Identical") -> staticDecoder(Identical),
    ID("Valid") -> staticDecoder(Valid),
    ID("Committed") -> commitDecoder.map(cmt => Committed(cmt).asInstanceOf[Thing]),

    ID("AlreadyExist") -> anythingDecoder.map(AlreadyExist),
    ID("Conflict") -> alwaysFail("TODO: Conflict"),
    ID("MultiError") -> alwaysFail("TODO: MultiError"),
    ID("NotImplemented") -> staticDecoder(NotImplemented),
    ID("NotSupported") -> staticDecoder(NotSupported),
  )

  val linkDecoders: Map[ID, Decoder[Thing]] = ts.typeSystem.collect { // Location, Temporal, Plain, Predicate, Agent
    case (id: ID, l@Link(_, _: AnyOf)) => id -> staticDecoder(l) // Message, In, Out, Query, Command, Report, Event, Error, CRUD
  } // total: 14

  val decoders: Map[ID, Decoder[Thing]] = {
    locationDecoders ++ compositeDecoders ++ messageDecoders ++ linkDecoders ++
      Map(
        ID("Anything") -> staticDecoder(Anything),
        ID("Nothing") -> staticDecoder(Nothing),
        ID("Data") -> alwaysFail("TODO Data"),
        ID("Node") -> nodeDecoder.map(_.asInstanceOf[Thing]),
      )
  }

  private def staticDecoder(what: Thing): Decoder[Thing] = Decoder { _: HCursor => Right(what) }
  private def alwaysFail(msg: String): Decoder[Thing] = Decoder { _: HCursor => Left(DecodingFailure(msg, List.empty)) }
}

object ThingDecoder {
  def thingDecoder(over: Thing): Decoder[Thing] = decoder(over).map(_.asInstanceOf[Thing])

  def decoder(thing: Thing): Decoder[_] = thing match {
      case p: Plain => plainDecoder(p)
      case Enlist(of) => Decoder.decodeSeq(decoder(of))
      case Index(Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, decoder(over))
      case t: Type => dataDecoder(t)
      case Link(p: Path, t: Type) => dataDecoder(t)
      case Link(p: Path, any: AnyOf) =>
        val options: Map[Location, Link] = any.p.collect {
          case l@Link(id: ID, Nothing) => id -> l
          case l@Link(pa: Path, Nothing) => pa.last -> l
        }.toMap

        Decoder.decodeString.map { s =>
          options.get(Location(s)) match {
            case Some(found) => found
            case None =>
                DecodingFailure.fromThrowable(
                  new IllegalArgumentException(s"$s Not a value from AnyOf"), List.empty)
          }
        }
      case Link(p: Path, other) =>
        throw new IllegalArgumentException(s"On $p not a type: $other")
      case many: AnyOf => anyDecoder(many)

      case Nothing => Decoder.apply(_ => Right(Nothing))
      case Anything => throw new IllegalArgumentException("Not supported, use AnyOf(<all things>) instead")
      case _ => ??? //TODO Predicate as AnyOf(<each predicate available>)
    }

  def dataDecoder(t: Type): Decoder[Data] = { c: HCursor =>
    t.fields.map {
      case Field(name, p) => c.downField(name).as(decoder(p))
    }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[_]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }.map { v => Data(t, v) }
  }

  private val plainDecoder: PartialFunction[Plain, Decoder[_]] = {
    case Bool => Decoder.decodeBoolean
    case Text(_) => Decoder.decodeString
    case Number(_, _) => Decoder.decodeLong
    case Date => Decoder.decodeString.map(sql.Date.valueOf)
    case Time => Decoder.decodeString.map(sql.Time.valueOf)
    case Timestamp => Decoder.decodeString.map(sql.Timestamp.valueOf)
    case other => throw new IllegalArgumentException(s"Not supported: $other")
  }

  def anyDecoder(many: AnyOf): Decoder[_] = { c: HCursor =>
    c.downField(discriminator).as[String].flatMap { name =>
      many.p.collect {
        case Link(p: Path, t: Type) if p.ids.last.key == name => dataDecoder(t).apply(c)
        case l@Link(p: Path, Nothing) if p.ids.last.key == name => Right(l) //use links for enums
      }.head
    }
  }
}
