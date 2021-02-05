package dev.rudiments.another.hardcore

import dev.rudiments.another.{In, Out, Tx}

import scala.reflect.ClassTag

class Pipeline[Rq <: In : ClassTag, I <: In, T <: Tx](f: Rq => (I, T)) extends PartialFunction[Rq, (I, T)] {
  override def isDefinedAt(rq: Rq): Boolean = g.isDefinedAt(rq)
  override def apply(rq: Rq): (I, T) = g.apply(rq)

  val g: PartialFunction[Rq, (I, T)] = { case rq: Rq => f(rq) }
}

class Drainage[O <: Out : ClassTag, T <: Tx : ClassTag, Rs <: Out](f: (O, T) => Rs) extends PartialFunction[(O, T), Rs] {
  override def isDefinedAt(rs: (O, T)): Boolean = g.isDefinedAt(rs)
  override def apply(rs: (O, T)): Rs = g.apply(rs)

  val g: PartialFunction[(O, T), Rs] = { case (out: O, tx: T) => f(out, tx) }
}

class Service[Rq <: In : ClassTag, I <: In, T <: Tx, O <: Out : ClassTag, Rs <: Out](
  p: Pipeline[Rq, I, T],
  s: PF,
  d: Drainage[O, T, Rs]
) extends PartialFunction[Rq, Rs] {

  override def isDefinedAt(rq: Rq): Boolean = f.isDefinedAt(rq)
  override def apply(rq: Rq): Rs = f.apply(rq)

  val f: PartialFunction[Rq, Rs] = {
    case rq: Rq =>
      val (in, tx) = p.apply(rq)
      s.apply((in, tx)) match {
        case o: O => d.apply((o, tx))
        case other => ???
      }
  }
}