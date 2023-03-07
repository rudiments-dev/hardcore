package dev.rudiments.hardcore

import java.lang

sealed trait Location extends Product {
  def /(l: Location): Location
  final def toIds: Seq[ID] = this match
    case Self => Seq.empty
    case id: ID => id :: Nil
    case path: Path => path.ids
}
object Location {
  def apply(ids: ID*): Location = ids match
    case Nil => Self
    case h :: Nil => h
    case _ => Path(ids:_*)
}

case object Self extends Location {
  def /(l: Location): Location = l match {
    case Self => Self
    case id: ID => id
    case path: Path => path
  }
}

final case class ID(key: Any) extends Location {
  def /(l: Location): Location = l match {
    case Self => this // or Path(id, Self)?
    case id: ID => Path(this, id)
    case path: Path => Path(this +: path.ids: _*)
  }
}

final case class Path(ids: ID*) extends Location {
  if(ids.size < 2)
    throw new IllegalArgumentException(s"Path should be at least with 2 IDs, but got: ${ids.size}")

  def /(l: Location): Location = l match {
    case Self => this // or Path(ids :+ Self)?
    case id: ID => Path(ids :+ id: _*)
    case path: Path => Path(ids ++ path.ids: _*)
  }

  def head: ID = ids.head
  def tail: Location =
    if (ids.size == 2) {
      ids.last
    } else {
      Path(ids.tail :_*)
    }

  override def toString: String = ids.map(_.key.toString).mkString("/")
}
