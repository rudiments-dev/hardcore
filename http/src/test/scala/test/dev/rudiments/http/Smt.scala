package test.dev.rudiments.http

sealed trait Blah {}

case class Smt(
  id: Long,
  name: String,
  comment: Option[String] = None
) extends Blah

case class Thng(
  code: String
) extends Blah