package dev.rudiments.hardcore

import dev.rudiments.hardcore.Node.partnersId

import scala.annotation.tailrec

class TypeSystem(tNode: Node) {
  val fromTypes: Map[Location, Thing] =
    tNode ??* Root match {
      case Found(_, values) => values - Root
      case other =>
        throw new IllegalStateException(s"Can't read types node, got $other")
    }

  val partners: Map[Location, Seq[Location]] = fromTypes.collect {
    case (l, n: Node) => l -> n.relations.getOrElse(partnersId, Seq.empty).collect {
      case p: Path => p.last
      case id: ID => id
    }
  }
  val types: Map[Location, Type] = fromTypes.collect { case (l, t: Type) => l -> t }
  val noThings: Set[Location] = fromTypes.collect { case (l, Nothing) => l }.toSet
  val partnersRelations: Seq[(Location, Location)] =
    partners.foldLeft(Seq.empty[(Location, Location)]) { case (acc, (from, to)) =>
      acc ++ to.map(t => from -> t)
    }
  val partnersRevIndex: Map[Location, Set[Location]] =
    partnersRelations.foldLeft(Map.empty[Location, Set[Location]]) { case (acc, (from, to)) =>
      acc.get(to) match {
        case Some(s) => acc ++ Map(to -> (s + from))
        case None => acc ++ Map(to -> Set(from))
      }
    }
  val predicates: Set[Location] = partners(ID("Predicate")).toSet

  def seal(): Map[Location, Predicate] = {
    val related = partners.collect {
      case p@(_, children) if (children.toSet -- noThings -- types.keys).isEmpty => p
    }
    val adt = related.map { case (parent, children) =>
        parent -> AnyOf(
          children
            .map { l => l -> fromTypes(l) }
            .collect {
              case (l, p: Predicate) => Link(l, p)
              case (l, _) => throw new IllegalStateException(s"Not a predicate on ${l}")
        }: _*)
    }
    val complex = fromTypes -- noThings -- types.keys -- adt.keys // Message, In, Out. TODO hierarchical
    val basic: Map[Location, Predicate] = adt ++ types
    val resolved = resolveComplex(complex, basic)

    val diff = fromTypes -- resolved.keys -- related.values.flatten.toSet
    if(diff.nonEmpty) {
      throw new IllegalStateException(s"Not all types are enum values or types or any of them: ${diff.keys.mkString(",")}")
    }
    resolved
  }

  @tailrec
  private def resolveComplex(
    todo: Map[Location, Thing],
    known: Map[Location, Predicate]
  ): Map[Location, Predicate] = {
    val done = todo.collect { case (l, _: Node) if (partners(l).toSet -- known.keys).isEmpty =>
      l -> AnyOf(partners(l).map(p => known(p)):_*)
    }
    if(done.size < todo.size) {
      resolveComplex(todo -- done.keys, known ++ done) // could be dangerous TODO stack limit
    } else {
      known ++ done
    }
  }

  val typeSystem: Map[Location, Predicate] = seal()
}
