package dev.rudiments.hardcore

sealed trait Message {}

sealed trait Event extends Message {}

case class Created[K, B, L](value: L | Tree[K, B, L]) extends Event
case class Updated[K, B, L](old: L | Tree[K, B, L], value: L | Tree[K, B, L]) extends Event
case class Deleted[K, B, L](old: L | Tree[K, B, L]) extends Event
case class Same[K, B, L](value: L | Tree[K, B, L]) extends Event
case class Readen[K, B, L](value: L | Tree[K, B, L]) extends Event
case class Commit[K](events: Seq[(List[K], Event)]) extends Event

sealed trait Error extends Message {
  final inline def throwIt(): Unit = throw asException
  final inline def asException: GotError = new GotError(this)
}
case class NotFound[K](path: List[K]) extends Error
case class LeafOnTheWay[K](k: K, path: List[K]) extends Error
case class Conflict(actual: Message, cause: Message) extends Error

final class GotError(err: Error) extends RuntimeException(err.toString) {}