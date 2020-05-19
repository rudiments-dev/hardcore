package dev.rudiments.hardcore.sql.parts

import dev.rudiments.hardcore.sql.schema.{Schema, Table}

case class From(schema: Schema, table: Table, as: Option[String])
