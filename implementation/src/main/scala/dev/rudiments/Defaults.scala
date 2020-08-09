package dev.rudiments

import java.sql.{Date, Timestamp}
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

object Defaults {
  val int: Int        = -1
  val long: Long      = -1
  val double: Double  = -1

  def today: Date     = Date.valueOf(LocalDate.now())
  def now: Timestamp  = Timestamp.valueOf(LocalDateTime.now())

  def uuid: UUID = UUID.randomUUID()
}
