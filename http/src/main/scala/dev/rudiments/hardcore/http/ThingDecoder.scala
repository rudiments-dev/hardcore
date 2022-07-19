package dev.rudiments.hardcore.http

import dev.rudiments.hardcore._
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

object ThingDecoder {
  def decoder(thing: Thing): Decoder[_] =
    thing match {
      case p: Plain => plainDecoder(p)
      case Enlist(of) => Decoder.decodeSeq(decoder(of))
      case Index(Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, decoder(over))
      case t: Type => dataDecoder(t)
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
    case other => throw new IllegalArgumentException(s"Not supported: $other")
  }
}
