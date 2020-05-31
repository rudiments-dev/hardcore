package dev.rudiments.hardcode.sql

import dev.rudiments.data.DataEvent
import dev.rudiments.hardcore.types.Type

trait SQL[Trans <: Transaction] {
  val softType: Type
  def exec(transaction: Trans): DataEvent
}

object SQL {
  def sequence[Trans <: Transaction](sqlSeq: Seq[SQL[Trans]], f: Seq[DataEvent] => DataEvent): SQL[Trans] = {
    val tt = sqlSeq.head.softType

    new SQL[Trans] {
      override val softType: Type = tt

      override def exec(transaction: Trans): DataEvent = {
        val dataEvents = sqlSeq.foldLeft(List.empty[DataEvent]){ case (acc, sql) => {
          acc :+ sql.exec(transaction)
        }}
        f(dataEvents)
      }
    }

  }

}