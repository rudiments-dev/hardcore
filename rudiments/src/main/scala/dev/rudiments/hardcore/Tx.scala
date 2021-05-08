package dev.rudiments.hardcore

import scala.collection.mutable

trait Tx extends Binary //TODO add state
case object NoTx extends Tx
trait LogTx extends Tx {
  val log: mutable.Seq[(In, Out)] = mutable.Seq.empty
}
final class LogOnlyTx extends LogTx

case object TxStart extends Out
