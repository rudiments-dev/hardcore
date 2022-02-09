package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.AnyWhere

import scala.collection.mutable

class Relation(val idIs: Predicate) extends AgentRead(idIs, List(AnyWhere)) {
  val relations: mutable.SeqMap[ID, Set[Path]] = mutable.SeqMap.empty

  def asGraph: Node = ???

  override def read(id: ID): Out = Relation.read(this).query(Read(id))

  override val skill: RW = Skill(
    Relation.read(this),
    Relation.add(this),
    Relation.find(this)
  )
}

object Relation {
  def read(implicit ctx: Relation): RO = RO {
    case Read(id) => ctx.relations.get(id) match {
      case Some(found: Set[Path]) => Readen(id, Data(List(AnyWhere), found))
      case None => NotFound(id)
    }
  }

  def add(implicit ctx: Relation): RW = RW (
    query = {
      case AddRelation(id, path) => ctx >> id match {
        case Readen(i, Data(List(AnyWhere), related: Set[Path])) =>
          if(related.contains(path)) {
            AlreadyRelated(i, path)
          } else {
            RelationAdded(i, path)
          }
        case NotFound(_) => RelationAdded(id, path)
        case e: Error => e
        case other => throw new IllegalArgumentException(s"Unexpected $other")
      }
    },
    write = {
      case RelationAdded(id, path) =>
        ctx.relations.get(id) match {
          case Some(v) =>
            ctx.relations += (id -> v.+(path)) //TODO hierarchy duplication check?
            Data(List(AnyWhere), ctx.relations(id))
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for relation $id")
          case None =>
            ctx.relations += id -> Set(path) //TODO hierarchy duplication check?
            Data(List(AnyWhere), ctx.relations(id))
        }
    }
  )

  def find(implicit ctx: Relation): RO = RO {
    case Find(All) => Found(
      All, ctx.relations.collect {
        case (id, rel) => id -> Data(List(AnyWhere), rel)
      }.toMap
    )
    case Find(p) => Found(
      p, ctx.relations.collect {
        case (id, v) if p.validate(v) => id -> Data(List(AnyWhere), v)
      }.toMap
    )
    case other => throw new IllegalArgumentException(s"Unexpected found: $other")
    //TODO compare Agents?
  }
}

trait Node extends ADT {}
case class Branch(ref: Ref, in: Set[Node], out: Set[Node]) extends Node
case class Leaf(ref: Ref) extends Node