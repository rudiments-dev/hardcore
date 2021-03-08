package dev.rudiments

import dev.rudiments.another.hardcore.{Commit, DataEvent, ID, Moved}

import java.sql.Timestamp
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
    val log: mutable.Seq[LogRecord] = mutable.Seq.empty

    def record(in: In, out: Out): LogRecord = {
      val record = LogRecord(in, out, new Timestamp(new java.util.Date().getTime))
      log :+ record
      record
    }

    def commit[T: ClassTag]: Commit[T] = {
      log.collect { case LogRecord(_, out: DataEvent[T], _) => out }
      Commit[T](
        log
          .collect { case LogRecord(_, out: DataEvent[T], _) => out }
          .foldLeft(mutable.Map.empty[ID[T], DataEvent[T]]) { (acc, item) =>
            item match {
              case DataEvent(id) => acc + (id -> item)
              case Moved(from, _, to, _) => acc - from; acc + (to -> item)
            }
            acc
          }.toMap
      )
    }
  }
  final class LogOnlyTx extends LogTx

  final case class LogRecord(in: In, out: Out, when: Timestamp)

  case object TxStart extends Out


}
