package dev.rudiments.hardcode.sql.schema

import dev.rudiments.domain.{DTO, Spec}

case class TypedSchema(name: String, tables: Map[Spec, Table], references: Set[FK]) extends DTO {}
