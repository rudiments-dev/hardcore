package dev.rudiments.hardcore.sql.parts

import dev.rudiments.hardcore.sql.schema.Table

case class From(table: Table, as: Option[String])
