package dev.rudiments.hardcore.sql.schema

import dev.rudiments.hardcore.types.DTO

case class Table(name: String, columns: Seq[Column], pk: Seq[Column]) extends DTO
