package dev.rudiments.codecs

import Coded.{Error, Ok}

import scala.reflect.ClassTag

class Encoder[A, B](en: A => Coded[B]) {
  def map[C](f: B => C): Encoder[A, C] = Encoder(en.andThen(_.map(f)))
}
object Encoder {
  def pure[A, B](f: A => B): Encoder[A, B] = Encoder(f.andThen(r => Ok(r)))
}

class Decoder[A, B](de: A => Decoded[B]) {
  def map[C](f: B => C): Decoder[A, C] = Decoder(de.andThen(_.map(f)))
}

class Codec[A, B](en: A => Coded[B], de: B => Decoded[A]) {
  def bimap[C](
    f: B => C, g: C => B
  ): Codec[A, C] = Codec(
    en.andThen(_.map(f)), g.andThen(de)
  )
}

enum Coded[A] {
  case Error(e: Exception)
  case Ok(value: A)

  def map[B](f: A => B): Coded[B] = this match {
    case Coded.Error(e) => Coded.Error(e)
    case Coded.Ok(v) => Coded.Ok(f(v))
  }
}

case class Encoded[A](value: Either[Exception, A]) {
  def map[B](f: A => B): Encoded[B] = Encoded(value.map(f))
}
case class Decoded[A](value: Either[Exception, A]) {
  def map[B](f: A => B): Decoded[B] = Decoded(value.map(f))
}
