package dev.rudiments.hardcode.sql.schema

import dev.rudiments.hardcore.types.{DTO, HardType, Type}

case class Schema(name: String, tables: Map[Type, Table], references: Set[FK]) extends DTO {}
