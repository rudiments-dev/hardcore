package dev.rudiments.hardcore

sealed trait Location {
  def / (other: Location): Location = (this, other) match {
    case (id1: ID, id2: ID) => Path(id1, id2)
    case (p: Path, id: ID) => Path((p.ids :+ id):_*)
    case (p1: Path, p2: Path) => Path((p1.ids :++ p2.ids):_*)
    case (id: ID, p: Path) => Path((id +: p.ids):_*)
    case (Root, id: ID) => id
    case (Root, p: Path) => p
    case (Root, Root) => Root
    case (p: Path, Root) => p
    case (id: ID, Root) => id
    case (_, _) => throw new IllegalArgumentException("prohibited")
  }

  def / (p: String): Location = this./(ID(p))
  def lastString: String
}
object Location extends Ordering[Location] {
  def apply(s: Seq[String]): Location = {
    s.size match {
      case 0 => Root
      case 1 => ID(s.head)
      case _ => Path(s.map(ID): _*)
    }
  }

  override def compare(x: Location, y: Location): Int = (x, y) match {
    case (Root, Root) => 0
    case (Root, _: ID) => 1
    case (Root, _: Path) => 1
    case (_: ID, Root) => -1
    case (_: Path, Root) => -1
    case (idX: ID, idY: ID) => Ordering[String].compare(idX.toString, idY.toString)
    case (_: ID, _: Path) => -1
    case (_: Path, _: ID) => 1
    case (pX: Path, pY: Path) =>
      if(pX.ids.size == pY.ids.size) {
        val pairs = pX.ids.zip(pY.ids)
        pairs.foldLeft(0) { (acc, pair) =>
          if (acc == 0) {
            compare(pair._1, pair._2)
          } else {
            acc
          }
        }
      } else {
        Ordering[Int].compare(pX.ids.size, pY.ids.size)
      }
  }
}
final case class ID(key: Any) extends Location {
  override def toString: String = key.toString
  override def lastString: String = key.toString
}
final case class Path(ids: ID*) extends Location {
  override def toString: String = ids.map(_.key).mkString("/")
  override def lastString: String = ids.last.lastString

  def dropHead: Location = ids.toList match {
    case head :: tail :: Nil => tail
    case head :: tail => Path(tail: _*)
    case other => Unmatched
  }

  def dropTail: Location = ids.toList match {
    case head :: tail :: Nil => head
    case head :: Nil => Unmatched
    case other => Path(other.dropRight(1): _*)
  }

  def head: ID = ids.head
  def tail: Location = ids.toList match {
    case head :: tail :: Nil => tail
    case head :: tail => Path(tail: _*)
    case head :: Nil => Root
    case Nil => throw new IllegalArgumentException("empty path")
  }
  def last: ID = ids.toList match {
    case head :: tail :: Nil => tail
    case head :: Nil => head
    case other => other.last
  }

  def drop(other: Location): Location = {
    other match {
      case Root => this
      case id: ID if this.last == id => this.dropTail
      case p: Path =>
        val dropped = this.ids.takeRight(p.ids.size)
        if (dropped == p.ids) {
          Path(this.ids.dropRight(p.ids.size):_*)
        } else {
          Unmatched
        }
    }
  }
}
case object Root extends Location {
  override def lastString: String = "/"
}
case object Unmatched extends Location {
  override def lastString: String = "Unmatched"
}
