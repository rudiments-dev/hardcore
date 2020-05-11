package dev.rudiments.hardcore.sql.parts

import dev.rudiments.hardcore.sql.schema.Column

case class Selector(column: Column, as: Option[String])
