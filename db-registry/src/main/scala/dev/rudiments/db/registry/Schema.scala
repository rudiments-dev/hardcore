package dev.rudiments.db.registry

import dev.rudiments.hardcode.sql.schema.{FK, Table}
import dev.rudiments.domain.DTO

case class Schema(name: String, tables: Set[Table], references: Set[FK]) extends DTO {}
