package dev.rudiments.hardcore.sql.schema

import dev.rudiments.hardcore.types.DTO

case class Column(name: String, `type`: ColumnType, nullable: Boolean, default: Boolean, pk: Boolean) extends DTO
