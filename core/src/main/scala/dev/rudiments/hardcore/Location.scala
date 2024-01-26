package dev.rudiments.hardcore

case class Location[T](where: T) {}
type ID = Location[String]
type Path[T <: Tuple] = Location[T]
