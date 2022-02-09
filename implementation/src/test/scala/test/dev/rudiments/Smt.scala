package test.dev.rudiments

import dev.rudiments.hardcore.ADT

sealed trait Blah extends ADT {}

case class Smt(
  id: Long,
  name: String,
  comment: Option[String]
) extends Blah

case class Thng(
  code: String
) extends Blah