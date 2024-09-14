package dev.rudiments.hardcore

import scala.reflect.ClassTag


case class Tree[K, B, L](
  self: B,
  items: Seq[(K, L | Tree[K, B, L])]
) {
  type Item = (List[K], L | B)
  type Node = (K, L | Tree[K, B, L])
  type T = Tree[K, B, L]

  val (leaves, branches) = {
    val (ls, bs) = items.partitionMap {
      case (id, t@Tree(_, _)) => Right[(K, L), (K, T)](id -> t.asInstanceOf[T])
      case (id, l: L) => Left[(K, L), (K, T)](id -> l)
      case _ => throw new IllegalStateException(s"Should never happen while indexing a tree")
    }
    (ls.toMap, bs.toMap)
  }

  require(
    leaves.keySet.intersect(branches.keySet).isEmpty,
    s"Branches and leaves have intersecting keys: ${leaves.keySet.intersect(branches.keySet)}"
  )

  val index: Map[K, L | T] = items.toMap



  // Search

  def deep(implicit  tK: ClassTag[K], tB: ClassTag[B], tL: ClassTag[L], tC: ClassTag[T]): Seq[Item] = {
    val rootK = List.empty[K]
    Seq(rootK -> self) ++ this.deep(rootK)
  }

  def deep(path: List[K])(implicit  tK: ClassTag[K], tB: ClassTag[B], tL: ClassTag[L], tC: ClassTag[T]): Seq[Item]  = {
    items.flatMap {
      case (k: K, t: T) => Seq((path :+ k) -> t.self) ++ t.deep(path :+ k)
      case (k: K, l: L) => Seq((path :+ k) -> l)
      case _ => throw new IllegalStateException(s"Should never happen in deep search")
    }
  }


  def wide(implicit  tK: ClassTag[K], tB: ClassTag[B], tL: ClassTag[L], tC: ClassTag[T]): Seq[Item] = {
    val rootK = List.empty[K]
    Seq(rootK -> self) ++ this.wide(rootK)
  }

  def wide(path: List[K])(implicit  tK: ClassTag[K], tB: ClassTag[B], tL: ClassTag[L], tC: ClassTag[T]): Seq[Item] = {
    items.map {
      case (k, t: T) => (path :+ k) -> t.self
      case (k, l: L) => (path :+ k) -> l
      case _ => throw new IllegalStateException(s"Should never happen in wide search")
    } ++ items.collect {
      case (k, t: Tree[K, B, L]) => t.wide(path :+ k)
    }.flatten
  }
}

object Tree {
  def onlyRoot[K, B, L](self: B) = new Tree(self, Seq.empty[(K, L | Tree[K, B, L])])
  def apply[K, L](items: (K, L | Tree[K, Unit, L])*) = new Tree[K, Unit, L]((), items.toSeq)
  def empty[K, L] = new Tree[K, Unit, L]((), Seq.empty)
}