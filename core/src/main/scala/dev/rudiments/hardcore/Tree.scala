package dev.rudiments.hardcore


case class Tree[K, B, L](
  self: B,
  items: Seq[(K, L | Tree[K, B, L])]
) {
  type Item = (List[K], L | B)
  type Node = (K, L | Tree[K, B, L])
  type T = Tree[K, B, L]
  type N = L | T

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

  val index: Map[K, N] = items.toMap


  def read(keys: List[K]): Either[Error, N] = keys match {
    case Nil => Right(this)
    case h :: Nil => read(h)
    case h :: p => this.index.get(h) match {
      case Some(t@Tree(_, _)) => t.asInstanceOf[T].read(p)
      case Some(l: L) => Left(LeafOnTheWay(h, p))
      case None => Left(NotFound(keys))
    }
  }

  def read(k: K): Either[Error, N] = this.index.get(k) match {
    case Some(t@Tree(_, _)) => Right(t)
    case Some(l: L) => Right(l)
    case None => Left(NotFound(k :: Nil))
  }

  def apply(keys: List[K], evt: Event): T = ???

  def apply(k: K, evt: Event): T = (this.index.get(k), evt) match {
    case (_, Same(_)) => this //TODO conflicts
    case (None, Created(v: N)) => new Tree(self, items :+ (k -> v))
    case (Some(i), c@Created(_)) => throw Conflict(Readen(i), c).asException
    case (Some(i), u@Updated(old, value)) => if(old == i) {
      val idx = items.indexOf(k -> old)
      if(idx == -1) { throw NotFound(k :: Nil).asException }
      val updated = items.updated(idx, k -> value.asInstanceOf[L | Tree[K, B, L]])
      new Tree(self, updated)
    } else {
      throw Conflict(Readen(i), u).asException
    }
    case (Some(i), d@Deleted(old)) => if(old == i) {
      val updated = items.filterNot(item => item == (k -> old))
      new Tree(self, updated)
    } else {
      throw Conflict(Readen(i), d).asException
    }
    case (Some(t@Tree(_, _)), Commit(events: Seq[(List[K], Event)])) =>
      events.foldLeft(t.asInstanceOf[T]) { case (tree, (p, e)) => tree.apply(p, e)}
    case (Some(i), evt) => throw Conflict(Readen(i), evt).asException
    case (None, evt) => throw Conflict(NotFound(k :: Nil), evt).asException
  }

  // Search

  def deep: Seq[Item] = {
    val rootK = List.empty[K]
    Seq(rootK -> self) ++ this.deep(rootK)
  }

  def deep(path: List[K]): Seq[Item]  = {
    items.flatMap {
      case (k: K, t@Tree(_, _)) => Seq((path :+ k) -> t.asInstanceOf[T].self) ++ t.asInstanceOf[T].deep(path :+ k)
      case (k: K, l: L) => Seq((path :+ k) -> l)
      case _ => throw new IllegalStateException(s"Should never happen in deep search")
    }
  }


  def wide: Seq[Item] = {
    val rootK = List.empty[K]
    Seq(rootK -> self) ++ this.wide(rootK)
  }

  def wide(path: List[K]): Seq[Item] = {
    items.map {
      case (k, t@Tree(_, _)) => (path :+ k) -> t.asInstanceOf[T].self
      case (k, l: L) => (path :+ k) -> l
      case _ => throw new IllegalStateException(s"Should never happen in wide search")
    } ++ items.collect {
      case (k, t@Tree(_, _)) => t.asInstanceOf[T].wide(path :+ k)
    }.flatten
  }
}

object Tree {
  def onlyRoot[K, B, L](self: B) = new Tree(self, Seq.empty[(K, L | Tree[K, B, L])])
  def apply[K, L](items: (K, L | Tree[K, Unit, L])*) = new Tree[K, Unit, L]((), items.toSeq)
  def empty[K, L] = new Tree[K, Unit, L]((), Seq.empty)
}
