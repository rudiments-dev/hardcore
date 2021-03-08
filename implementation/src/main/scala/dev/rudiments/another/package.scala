package dev.rudiments

import scala.collection.mutable
import scala.reflect.ClassTag

package object another {
  trait ADT extends Product // only DTO, serializable data allowed
  trait Binary // for all non-ADT references

  trait Message extends ADT

  trait Request extends Message
  trait Response extends Message

  trait In extends Message
  trait Out extends Message {
    def flatMap[O <: Out : ClassTag](f: O => Out): Out = this match {
      case m: O => f(m)
      case other => other
    }

    def when[O <: Out : ClassTag](f: O => Unit): Out = this match {
      case m: O =>
        f(m)
        this
      case _ => this
    }
  }

  trait Command extends In
  trait Query extends In

  trait Event extends Out
  trait Report extends Out
  trait Error extends Out

  trait Tx extends Binary //TODO add state
  case object NoTx extends Tx
  trait LogTx extends Tx {
    val log: mutable.Seq[(In, Out)] = mutable.Seq.empty
  }
  final class LogOnlyTx extends LogTx

  case object TxStart extends Out


}
