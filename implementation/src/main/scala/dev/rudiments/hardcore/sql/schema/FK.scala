package dev.rudiments.hardcore.sql.schema

import dev.rudiments.hardcore.types.DTO

case class FK(from: Table, to: Table, references: Map[Column, Column]) extends DTO {
  override def toString: String = {
    val refs = references.toSeq
    from.name + "(" + refs.map(_._1).mkString(", ") + ") -> " + to.name + refs.map(_._2).mkString(", ") + ")"
  }
}