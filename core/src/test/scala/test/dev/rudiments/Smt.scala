package test.dev.rudiments

sealed trait Blah {}

case class Smt(
  id: Long,
  name: String,
  comment: Option[String]
) extends Blah

case class Thng(
  code: String
) extends Blah