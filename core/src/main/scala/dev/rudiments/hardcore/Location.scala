package dev.rudiments.hardcore

sealed trait Location extends Product {
  def /(l: Location): Location
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
  def /(l: Location): Location = l match {
    case Self => this // or Path(ids :+ Self)?
    case id: ID => Path(ids :+ id: _*)
    case path: Path => Path(ids ++ path.ids: _*)
  }
}
