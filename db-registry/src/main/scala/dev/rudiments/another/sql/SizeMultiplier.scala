package dev.rudiments.another.sql

sealed trait SizeMultiplier

object SizeMultipliers {

  case object N extends SizeMultiplier
  case object K extends SizeMultiplier
  case object M extends SizeMultiplier
  case object G extends SizeMultiplier
  case object T extends SizeMultiplier
  case object P extends SizeMultiplier
}