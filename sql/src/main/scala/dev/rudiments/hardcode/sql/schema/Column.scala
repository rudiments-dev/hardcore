package dev.rudiments.hardcode.sql.schema

import dev.rudiments.domain.DTO

case class Column(name: String, `type`: ColumnType, nullable: Boolean, default: Boolean, pk: Boolean) extends DTO
