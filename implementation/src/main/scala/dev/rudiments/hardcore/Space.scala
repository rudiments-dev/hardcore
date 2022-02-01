package dev.rudiments.hardcore

import scala.reflect.ClassTag

class Space() extends AgentRead(All, All) { //TODO fix by using separate predicates, not based on space types
  val root: Memory = new Memory(All, All)
  override val f: PartialFunction[In, Out] = root.f
  override val skill: RW = root.skill
  override def read(id: ID): Out = root.read(id)

  override def <<(what: In): Out = root << what

  Type.init(this)

  def find[T <: Thing : ClassTag](path: Path): T = {
    path.ids.foldLeft(root.asInstanceOf[Thing]) { (dir, id) =>
        dir match {
          case a: AgentRead => flatten(a >> id)
          case a: Agent => flatten(a << Read(id))
          case t: T => t //really?
          case other => throw new IllegalArgumentException(s"Not supported: $other")
        }
    } match {
      case t: T => t
      case other => throw new IllegalArgumentException(s"Not supported: $other")
    }
  }

  def flatten[T <: Thing : ClassTag](out: Out): Thing = out match {
    case Readen(_, memory: Memory) => memory
    case Readen(_, a: Agent) => a
    case Readen(_, t: T) => t
    case NotFound(id) => throw new IllegalArgumentException(s"No $id in location")
  }

  def apply(path: Path, what: In): Out = find[Agent](path).apply(what)

  def add(path: Path, id: ID, what: Thing): Unit = path.ids.foldLeft(root.asInstanceOf[Agent]) {
    case (dir: Memory, i) =>
      dir >> id match {
        case Readen(_, memory: Memory) => memory
        case NotFound(_) =>
          val m = new Memory(All, All)
          dir << Create(i, m)
          m
      }
  } << Create(id, what)
}
