package dev.rudiments.hardcore

class Space() extends Agent(Type.build[In], Type.build[Out]) {
  val root: Memory = new Memory()
  override val f: PartialFunction[In, Out] = root.f
  override val skill: RW = root.skill

  def find(path: Path): Memory = { //TODO Thing or at least agent instead of memory
    path.ids.foldLeft(root) { (dir, id) =>
      dir.read(id) match {
        case Readen(_, memory: Memory) => memory
        case NotFound(_) => throw new IllegalArgumentException(s"No #$id in location")
      }
    }
  }

  def apply(path: Path, what: In): Out = find(path).apply(what)

  def add(path: Path, id: ID, what: Thing): Unit = path.ids.foldLeft(root.asInstanceOf[Agent]) {
    case (dir: Memory, i) =>
      dir.read(id) match {
        case Readen(_, memory: Memory) => memory
        case NotFound(_) =>
          val m = new Memory()
          dir(Create(i, m))
          m
      }
  }.apply(Create(id, what))
}
