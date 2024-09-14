package dev.rudiments.hardcore

sealed trait Message[K] {
  val path: List[K]
}

sealed trait Event[K] extends Message[K] {}

case class Created[K, B, L](path: List[K], value: L | Tree[K, B, L]) extends Event[K]
case class Updated[K, B, L](path: List[K], old: L | Tree[K, B, L], value: L | Tree[K, B, L]) extends Event[K]
case class Deleted[K, B, L](path: List[K], old: L | Tree[K, B, L]) extends Event[K]
case class Same[K, B, L](path: List[K], value: L | Tree[K, B, L]) extends Event[K]
case class Commit[K](path: List[K], events: Seq[Event[K]]) extends Event[K]

sealed trait Error[K] extends Message[K] {}
case class NotFound[K](path: List[K]) extends Error[K]
case class LeafOnTheWay[K](k: K, path: List[K]) extends Error[K]
