package dev.rudiments.hardcore

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
}
case object Root extends Location
