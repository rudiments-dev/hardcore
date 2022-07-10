package dev.rudiments.hardcore

import scala.reflect.ClassTag

sealed trait Location {
  def / (other: Location): Location = (this, other) match {
    case (id1: ID, id2: ID) => Path(id1, id2)
    case (p: Path, id: ID) => Path((p.ids :+ id):_*)
    case (p1: Path, p2: Path) => Path((p1.ids :++ p2.ids):_*)
    case (id: ID, p: Path) => Path((id +: p.ids):_*)
    case (Root, id: ID) => id
    case (Root, p: Path) => p
    case (_, _) => throw new IllegalArgumentException("/Root prohibited")
  }
}
final case class ID(key: Any) extends Location {
  override def toString: String = key.toString
}
final case class Path(ids: ID*) extends Location {
  override def toString: String = ids.map(_.key).mkString("/")

  def -/ (what: ID): Location = ids.toList match {
    case head :: tail :: Nil if head == what => tail
    case head :: tail if head == what => Path(tail: _*)
    case other => Unmatched
  }
}
case object Root extends Location
case object Unmatched extends Location


case class Node[T : ClassTag](
  self: Option[T],
  leafs: Map[ID, T],
  branches: Map[ID, Node[T]]
) {
  def add(pair: (Location, T)): Node[T] = {
    pair match {
      case (Root, t: T) if this.self.isEmpty => new Node(Some(t), this.leafs, this.branches)
      case (Root, _) => throw new IllegalArgumentException("Root already exist in that Node")
      case (id: ID, t: T) => new Node[T](this.self, this.leafs + (id -> t), this.branches)
      case (path: Path, t: T) =>
        val h = path.ids.head
        val rest = path -/ h
        this.branches.get(h) match {
          case Some(existing) => new Node[T](
            this.self,
            this.leafs,
            this.branches + (h -> existing.add(rest -> t))
          )
          case None => new Node[T](
            this.self,
            this.leafs,
            this.branches + (h -> Node.wrap(rest, t))
          )
        }
    }
  }

  def find(where: Location): Either[Location, T] = where match {
    case Root => this.self match {
      case Some(t) => Right(t)
      case None => Left(Root)
    }
    case id: ID => this.leafs.get(id) match {
      case Some(found) => Right(found)
      case None => Left(id)
    }
    case p: Path =>
      val h = p.ids.head
      if (leafs.contains(h)) {
        Left(p)
      } else {
        branches.get(h) match {
          case Some(existing) => existing.find(p -/ h)
          case None => Left(p)
        }
      }
    case other => Left(other)
  }

  def seek(where: Location): Option[Node[T]] = where match {
    case id: ID => this.branches.get(id)
    case path: Path => path.ids.foldLeft(Option(this)) { (acc, el) =>
      acc.flatMap(_.branches.get(el))
    }
    case Root => Some(this)
    case Unmatched => None
  }
}

object Node {
  def empty[T : ClassTag]: Node[T] = new Node(None, Map.empty, Map.empty)

  def wrap[T : ClassTag](l: Location, t: T): Node[T] = l match {
    case Root => new Node[T](Some(t), Map.empty, Map.empty)
    case id: ID => new Node[T](None, Map(id -> t), Map.empty)
    case path: Path =>
      val h = path.ids.head
      new Node[T](None, Map.empty, Map(h -> wrap(path -/ h, t)))

    case other => throw new IllegalArgumentException(s"$other not supported in Node")
  }

  def fromMap[T : ClassTag](from: Map[Location, T]): Node[T] = {
    from.foldLeft(Node.empty[T]) { (acc, el) => acc.add(el) }
  }
}