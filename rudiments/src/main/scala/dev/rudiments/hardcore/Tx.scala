package dev.rudiments.hardcore

import scala.collection.mutable

class Tx {
  val story: mutable.Seq[(In, Out)] = mutable.Seq.empty
  def apply(in: In, out: Out): Out = {
    story :+ in -> out
    out
  }
}

case object TxStart extends Out
