package dev.rudiments.hardcode.sql.schema

import dev.rudiments.hardcore.types.{DTO, Type}

case class TypedSchema(name: String, tables: Map[Type, Table], references: Set[FK]) extends DTO {}
