package dev.rudiments.app

import dev.rudiments.hardcore.ADT

case class Body (
  name: String,
  strings: Seq[String]
) extends ADT
