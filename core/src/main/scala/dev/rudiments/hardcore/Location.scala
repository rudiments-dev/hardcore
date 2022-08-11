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
object Location {
  def apply(s: Seq[String]): Location = {
    s.size match {
      case 0 => Root
      case 1 => ID(s.head)
      case _ => Path(s.map(ID): _*)
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

  def -/ (what: ID): Location = ids.toList match {
    case head :: tail :: Nil if head == what => tail
    case head :: tail if head == what => Path(tail: _*)
    case other => Unmatched
  }
}
case object Root extends Location {
  override def lastString: String = "/"
}
case object Unmatched extends Location {
  override def lastString: String = "Unmatched"
}
