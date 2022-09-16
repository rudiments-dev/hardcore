package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.Predicate.Anything
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingDecoder.{dataDecoder, predicateDecoder}
import dev.rudiments.hardcore.http.ThingEncoder.discriminator
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

import java.sql

class ThingDecoder(tNode: Node) {
  private val ts = new TypeSystem(tNode).typeSystem

  def discriminatedDecoder: Decoder[Thing] = { c: HCursor =>
    c.downField(discriminator).as[String].flatMap { s =>
      val l = Location(s)

      ts.get(l) match {
        case Some(p: Predicate) => ???
        case None => Left(DecodingFailure(s"Not in /types: $s", List.empty))
      }
    }
  }

  /*
  if(ts.predicates.contains(l)) {
    predicateDecoder(l)
  } else if(ts.partners.contains(l)) {
    ts.fromTypes.get(l) match {
      case Some(found) => found match {
        case Nothing => ???
      }
      case None => Left(DecodingFailure(s"Not found $l in type system", List.empty))
    }
  } else if (ts.noThings.contains(l)) {
    Right(Link(l, Nothing))
  } else if (ts.types.contains(l)) {
    dataDecoder(ts.types(l)).apply(c)
  } else {
    Left(DecodingFailure(s"Not in /types: $s", List.empty))
  }
  * */

  val nodeDecoder: Decoder[Node] = { c: HCursor =>
    val keys = c.keys.getOrElse(Seq.empty).toSet

    ???
  }

  private def plainDecoder: Decoder[Plain] = { c: HCursor =>
    ???
  }
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

  def dataDecoder(t: Type): Decoder[Data] = dataValuesDecoder(t).map(values => Data(t, values))

  private def dataValuesDecoder(t: Type): Decoder[scala.List[_]] = { c: HCursor =>
    t.fields.map {
      case Field(name, p) => c.downField(name).as(decoder(p))
    }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[_]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
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

  private def predicateDecoder(l: Location): Decoder.Result[Predicate] = l match {
    case ID("Bool") => Right(Bool)
    case ID("Anything") => Right(Anything)
    case ID("Nothing") => Right(Nothing)

    case ID("Type") => ??? //hcursor and apply?
    case ID("Enlist") => ??? //hcursor and apply?
    case ID("Index") => ??? //hcursor and apply?
    case ID("AnyOf") => ??? //hcursor and apply?
    //TODO plain
  }


}
