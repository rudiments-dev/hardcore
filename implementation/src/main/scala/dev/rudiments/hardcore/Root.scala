package dev.rudiments.hardcore

class Root {
  val root: Memory = new Memory()

  def find(path: Path): Memory = {
    path.ids.foldLeft(root) { (dir, id) =>
      dir.state.get(id) match {
        case Some(memory: Memory) => memory
        case None => throw new IllegalArgumentException(s"No #$id in location")
      }
    }
  }

  def apply(path: Path, what: In): Out = find(path).apply(what)

  def add(path: Path, id: ID, what: Thing): Unit = path.ids.foldLeft(root) { (dir, i) =>
    dir.state.get(i) match {
      case Some(memory: Memory) => memory
      case None =>
        val m = new Memory()
        dir.apply(Create(i, m))
        m
    }
  }.apply(Create(id, what))
}
