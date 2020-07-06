package dev.rudiments.hardcode.sql.schema

import dev.rudiments.types.DTO

case class Column(name: String, `type`: ColumnType, nullable: Boolean, default: Boolean, pk: Boolean) extends DTO
