package dev.rudiments.hardcore.sql.schema

import dev.rudiments.hardcore.types.DTO

case class Table(name: String, columns: Seq[Column]) extends DTO {
  val pk: Seq[Column] = columns.filter(_.pk)
}
