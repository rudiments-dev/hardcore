package dev.rudiments.codecs

import dev.rudiments.utils.Log

import scala.compiletime.{constValue, erasedValue, error, summonFrom}
import scala.deriving.Mirror

object MJ extends Log {
  type En[A] = OneWay[A, TS]

  given intToNumber: OneWay[Int, TS.Number] = OneWay(i => Result.Ok(TS.Number(i)))
  given strToText: OneWay[String, TS.Text] = OneWay(s => Result.Ok(TS.Text(s)))
  given many[S, T <: TS](using t: OneWay[S, T]): OneWay[Iterable[S], TS.Many] = OneWay(l =>
    l.foldLeft(Result.Ok[TS.Many](TS.Many(Seq.empty))) { (acc, i) =>
      for {
        el <- t.t(i)
        many <- acc
      } yield TS.Many(many.of :+ el)
    }
  )
  given index[K, V, T <: TS](using keys: OneWay[K, TS.Text], values: OneWay[V, T]): OneWay[Map[K, V], TS.Idx] = OneWay( m =>
    m.foldLeft(Result.Ok[TS.Idx](TS.Idx(Map.empty))) { case (acc, (k, v)) =>
      for {
        key <- keys.t(k)
        value <- values.t(v)
        many <- acc
      } yield TS.Idx(many.of + (key -> value))
    }
  )

  inline final def summonLabelsRec[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]
  }

  inline final def summonEncoder[A]: En[A] = summonFrom {
    case encodeA: En[A] => encodeA
    case _: Mirror.Of[A] => derived[A]
  }

  inline final def summonEncodersRec[A <: Tuple]: List[En[_]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonEncoder[t] :: summonEncodersRec[ts]
    }

  inline final def derived[A](using A: Mirror.Of[A]): En[A] = {
    val name = constValue[A.MirroredLabel].asInstanceOf[String]
    val labels = summonLabelsRec[A.MirroredElemLabels].toArray
    val encoders = summonEncodersRec[A.MirroredElemTypes].toArray


    log.info("{} with labels {}", name, labels.mkString("[", ", ", "]"))
    ???
  }
}

enum TS {
  case Number(i: Int)
  case Text(s: String)
  case Many(of: Seq[TS])
  case Idx(of: Map[TS.Text, TS])
}