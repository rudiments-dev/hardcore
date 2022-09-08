package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.Predicate.Anything
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder.discriminator
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

import java.{lang, sql}

object ThingDecoder {
  def thingDecoder(over: Thing): Decoder[Thing] = over match {
    case p: Plain => plainDecoder(p).map(_.asInstanceOf[Thing])
    case t: Type => dataDecoder(t).map(_.asInstanceOf[Thing])
    case Link(p: Path, t: Type) if p.ids.head == ID("types") => dataDecoder(t).map(_.asInstanceOf[Thing])
    case Link(p: Path, _) => throw new IllegalArgumentException(s"Link not pointing '/types': $p")
    case many: AnyOf => anyDecoder(many).map(_.asInstanceOf[Thing])

    case Anything => Decoder.failed(
      DecodingFailure.fromThrowable(
        new IllegalArgumentException("Not supported, use AnyOf(<all things>) instead"), List.empty)
    )
    case _ => ??? //TODO Predicate as AnyOf(<each predicate available>)
  }

  def decoder(thing: Thing): Decoder[_] = thing match {
      case p: Plain => plainDecoder(p)
      case Enlist(of) => Decoder.decodeSeq(decoder(of))
      case Index(Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, decoder(over))
      case t: Type => dataDecoder(t)
      case Link(p: Path, t: Type) => dataDecoder(t)
      case Link(p: Path, other) =>
        throw new IllegalArgumentException(s"On $p not a type: $other")
      case many: AnyOf => anyDecoder(many)

      case Anything => throw new IllegalArgumentException("Not supported, use AnyOf(<all things>) instead")
      case _ => ??? //TODO Predicate as AnyOf(<each predicate available>)
    }

  def dataDecoder(t: Type): Decoder[Data] =
    dataValuesDecoder(t).map(values => Data(t, values))

  private def dataValuesDecoder(t: Type): Decoder[scala.List[_]] = (c: HCursor) => t.fields.map {
    case Field(name, p) => c.downField(name).as(decoder(p))
  }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[_]]) {
    (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
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

  def anyDecoder(many: AnyOf): Decoder[_] = new Decoder[Any] {
    override def apply(c: HCursor): Result[Any] = {
      c.downField(discriminator).as[String].flatMap { name =>
        many.p.collect {
          case Link(p: Path, t: Type) if p.ids.last.key == name => dataDecoder(t).apply(c)
          case l@Link(p: Path, Nothing) if p.ids.last.key == name => Right(l) //use links for enums
        }.head
      }
    }
  }
}
