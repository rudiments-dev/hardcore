package dev.rudiments.hardcore

import dev.rudiments.hardcore.Graph.Edges

case class EdgeTree[K, +B, +L, +E](
  leafs: Map[List[K], L],
  branches: Map[List[K], B],
  edges: Edges[List[K], E]
) {
  lazy val edgesFrom: Map[List[K], Edges[List[K], E]] = edges.groupBy(_.from)
  lazy val edgesTo: Map[List[K], Edges[List[K], E]] = edges.groupBy(_.to)

  if (leafs.keySet.intersect(branches.keySet).nonEmpty) {
    throw new IllegalArgumentException(s"Branches and leafs have intersecting keys: ${leafs.keySet.intersect(branches.keySet)}")
  }
  if (((edgesFrom.keySet ++ edgesTo.keySet) -- (leafs.keySet ++ branches.keySet)).nonEmpty) {
    throw new IllegalArgumentException(s"Edges have external keys")
  }

  def toGraph[E2 >: E](defaultEdge: B => E2): Graph[List[K], B | L, E] = {
    val defaultEdges = branches.map { case (k, v) => Graph.Edge(k, k, defaultEdge(v).asInstanceOf[E]) }.toSeq
    Graph(
      branches ++ leafs,
      defaultEdges ++ edges
    )
  }
}

object EdgeTree {

}