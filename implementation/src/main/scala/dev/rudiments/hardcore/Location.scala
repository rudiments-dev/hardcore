package dev.rudiments.hardcore

trait Location {}
final case class ID(key: Any) extends Location
final case class Path(ids: ID*) extends Location
