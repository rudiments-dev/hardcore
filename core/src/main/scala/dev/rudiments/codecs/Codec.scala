package dev.rudiments.codecs

import scala.reflect.ClassTag

class Encoder[A, B](val en: A => Result[B]) extends OneWay(en) {
  def apply(a: A): Result[B] = this.en(a)
  def toCodec(de: B => Result[A]): Codec[A, B] = Codec(en, de)
}
object Encoder {
  def apply[A, B](f: A => B): Encoder[A, B] = new Encoder(f.andThen(r => Result.Ok(r)))
  //TODO if error in B
}

class Decoder[A, B](val de: B => Result[A]) extends Encoder[B, A](de) {}
object Decoder {
  def apply[A, B](f: B => A): Decoder[A, B] = new Decoder(f.andThen(r => Result.Ok(r)))
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

  def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case Result.Error(e) => Result.Error(e)
    case Result.Ok(v) => f(v)
  }
}

class OneWay[A, B](t: A => Result[B]) {
  def map[C](f: B => C): OneWay[A, C] = OneWay(t.andThen(_.map(f)))
}
