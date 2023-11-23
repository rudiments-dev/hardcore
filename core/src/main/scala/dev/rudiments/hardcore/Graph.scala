package dev.rudiments.hardcore

import dev.rudiments.codecs.{Codec, Decoder, Encoder, Result}
import dev.rudiments.hardcore.Graph.{AroundNode, Edge, Edges, Item, SeqGraph}

case class Graph[K, +N, +E](
  nodes: Map[K, N],
  edges: Edges[K, E]
) {
  lazy val edgesFrom: Map[K, Edges[K, E]] = edges.groupBy(_.from)
  lazy val edgesTo: Map[K, Edges[K, E]] = edges.groupBy(_.to)

  def roots(f: Edge[K, E] => Boolean = _ => true): Set[K] =
    edgesTo.filter { case (k, v) => !v.exists(f) }.keySet

  def map[A, B](f: N => A, g: E => B): Graph[K, A, B] = Graph(
    nodes.map((k, v) => k -> f(v)),
    edges.map(e => e.copy(value = g(e.value)))
  )
  def collect[A, B](f: PartialFunction[AroundNode[K, N, E], AroundNode[K, A, B]]): Graph[K, A, B] = {
    val newNodes = nodes
      .map { case (k, n) => AroundNode(
        key = k,
        node = n,
        from = edgesFrom.getOrElse(k, Seq.empty),
        to = edgesTo.getOrElse(k, Seq.empty)
      )}
      .flatMap { a => if (f.isDefinedAt(a)) Some(f.apply(a)) else None }
      .toSeq

    val from = newNodes.flatMap(a => a.from)
    val to = newNodes.flatMap(a => a.to)
    val extra = to.toSet -- from.toSet

    Graph(
      newNodes.map(a => a.key -> a.node).toMap,
      from ++ to.filter(extra.contains)
    )
  }

  def filter(f: (K, N) => Boolean): Graph[K, N, E] = {
    val n = nodes.filter{ case (k, v) => f(k, v) }
    val keys = n.keySet
    Graph(
      nodes = n,
      edges = this.edges.filter(e => keys.contains(e.to) && keys.contains(e.from))
    )
  }

  def isSealed: Boolean = disjointed.isEmpty

  def disjointed: Edges[K, E] = {
    val fromKeys = edgesFrom.keySet
    val toKeys = edgesTo.keySet
    edges.filterNot(e => fromKeys.contains(e.from)) ++ edges.filterNot(e => toKeys.contains(e.to))
  }

  def join[N1 >: N, E1 >: E](that: Graph[K, N1, E1], joints: Edges[K, E1] = Seq.empty): Graph[K, N1, E1] = Graph(
    nodes = this.nodes ++ that.nodes,
    edges = this.edges ++ that.edges ++ joints
  )
  def split(keys: Set[K]): (Graph[K, N, E], Edges[K, E], Graph[K, N, E]) = {
    val cutNodeKeys = this.nodes.keySet -- keys
    val joints = this.edges.filter { case Edge(from, to, _) =>
      cutNodeKeys.contains(from) && keys.contains(to) || keys.contains(from) && cutNodeKeys.contains(to)
    }
    (this.filter { (k, _) => keys.contains(k) }, joints, this.filter { (k, _) => cutNodeKeys.contains(k) })
  }

  def to[A, N1 >: N, E1 >: E](using en: Encoder[Graph[K, N1, E1], A]): Result[A] = en.en(this)
}

object Graph {
  type Edges[A, +B] = Seq[Edge[A, B]]

  case class Edge[K, +E](from: K, to: K, value: E)
  case class Item[K, +N, +E](key: K, node: N, edges: Seq[(Int, E)]) // is a monadic cus it is an element in array!
  case class AroundNode[K, +N, +E](key: K, node: N, from: Seq[Edge[K, E]], to: Seq[Edge[K, E]])

  def empty[K, N, E]: Graph[K, N, E] = Graph(Map.empty[K, N], Seq.empty[Edge[K, E]])

  case class SeqGraph[K, N, E](
    items: Seq[Item[K, N, E]],
    keys: Map[K, Int],
  ) {
    def to[A](using en: Encoder[SeqGraph[K, N, E], A]): Result[A] = en.en(this)
  }

  def toSeqGraph[K, N, E](g: Graph[K, N, E]): SeqGraph[K, N, E] = {
    val indexed = g.nodes.toSeq.zipWithIndex
    val keys = indexed.map { case ((k, n), i) => k -> i }.toMap
    SeqGraph(
      indexed.map { case ((k, n), i) =>
        Item[K, N, E](k, n, g.edgesFrom.getOrElse(k, Seq.empty).map(e => keys(e.to) -> e.value))
      }, keys
    )
  }
}
