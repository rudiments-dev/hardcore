package dev.rudiments.hardcode.sql.schema

import dev.rudiments.types.DTO

case class Table(name: String, columns: Seq[Column]) extends DTO {
  val pk: Seq[Column] = columns.filter(_.pk)
}
