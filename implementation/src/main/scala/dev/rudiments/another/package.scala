package dev.rudiments

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
  }

  trait Command extends In
  trait Query extends In

  trait Event extends Out
  trait Report extends Out
  trait Error extends Out

  trait Tx extends Binary //TODO add state
  object NoTx extends Tx


}
