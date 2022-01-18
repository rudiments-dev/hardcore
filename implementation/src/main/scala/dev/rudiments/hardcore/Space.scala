package dev.rudiments.hardcore

class Space() extends Agent(All, All) { //TODO fix by using separate predicates, not based on space types
  val root: Memory = new Memory(All, All)
  override val f: PartialFunction[In, Out] = root.f
  override val skill: RW = root.skill

  Type.init(this)

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
          val m = new Memory(All, All)
          dir(Create(i, m))
          m
      }
  }.apply(Create(id, what))
}
