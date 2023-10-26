package dev.rudiments.codecs

import scala.reflect.ClassTag

class Encoder[A, B](en: A => Result[B]) extends OneWay(en) {
  def toCodec(de: B => Result[A]): Codec[A, B] = Codec(en, de)
}
object Encoder {
  def pure[A, B](f: A => B): Encoder[A, B] = Encoder(f.andThen(r => Result.Ok(r)))
  //TODO if error in B
}

class Decoder[A, B](de: A => Result[B]) extends OneWay(de){
  def toCodec(en: B => Result[A]): Codec[B, A] = Codec(en, de)
}

class Codec[A, B](en: A => Result[B], de: B => Result[A]) {
  def bimap[C](
    f: B => C, g: C => B
  ): Codec[A, C] = Codec(
    en.andThen(_.map(f)), g.andThen(de)
  )
}

enum Result[A] {
  case Error(e: Exception)
  case Ok(value: A)

  def map[B](f: A => B): Result[B] = this match {
    case Result.Error(e) => Result.Error(e)
    case Result.Ok(v) => Result.Ok(f(v))
  }
}

class OneWay[A, B](t: A => Result[B]) {
  def map[C](f: B => C): OneWay[A, C] = OneWay(t.andThen(_.map(f)))
}
