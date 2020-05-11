package dev.rudiments.hardcore.sql

trait SQL[T] {

  def exec(): T
}
