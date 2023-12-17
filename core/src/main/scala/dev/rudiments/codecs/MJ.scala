package dev.rudiments.codecs

object MJ {
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
}

enum TS {
  case Number(i: Int)
  case Text(s: String)
  case Many(of: Seq[TS])
  case Idx(of: Map[TS.Text, TS])
}