package dev.rudiments.hardcore

class Tx {

}

object Tx {}

//TODO naming
case class Apply(log: Seq[In]) extends Command
object Apply {
  def plain(log: Seq[In]): Seq[(ID, In)] = {
    log.foldLeft(Seq.empty[(ID, In)]) { (acc, cmd) =>
      cmd match {
        case c: CRUD => acc :+ c.id -> c
        case c: Apply => acc ++ plain(c.log)
        case _ => acc
      }
    }
  }

  def collapse(log: Seq[In]): Map[ID, In] = {
    plain(log).groupMapReduce(_._1)(_._2) {
      case (Create(id1, _), Update(id2, d2)) if id1 == id2 => Create(id1, d2)
      case (Create(id1, _), Delete(id2))     if id1 == id2 => Delete(id1) //TODO do nothing, check NotFound?
      case (Update(id1, _), Update(id2, d2)) if id1 == id2 => Update(id1, d2)
      case (Update(id1, _), Delete(id2))     if id1 == id2 => Delete(id1)
      case (Delete(id1),    Create(id2, d2)) if id1 == id2 => Update(id1, d2)
      case (_, _) => throw new IllegalArgumentException("Conflict")
    }
  }
}

case class Commit(delta: Map[ID, Event], extra: Seq[(In, Out)] = Seq.empty) extends Event

object Commit {
  def apply(events: Seq[(In, Out)]): Commit = {
    val data = collapse(events)
    val filtered = events.filter {
      case (_, _: Created) => false
      case (_, _: Updated) => false
      case (_, _: Deleted) => false
      case (_, _: Commit) => false
    }
    new Commit(data, filtered)
  }

  def plain(events: Seq[(In, Out)]): Seq[(ID, Event)] = {
    events.foldLeft(Seq.empty[(ID, Event)]) { (acc, cmdevt) =>
      cmdevt._2 match {
        case c: Created => acc :+ c.id -> c
        case c: Updated => acc :+ c.id -> c
        case c: Deleted => acc :+ c.id -> c
        case c: Commit => acc ++ c.delta ++ plain(c.extra)
        case _ => acc
      }
    }
  }

  def collapse(events: Seq[(In, Out)]): Map[ID, Event] = {
    plain(events).groupMapReduce(_._1)(_._2) {
      case (Created(id1, d1),       Updated(id2, d21, d22)) if id1 == id2 && d1 == d21  => Created(id1, d22)
      case (Created(id1, d1),       Deleted(id2, d2))       if id1 == id2 && d1 == d2   => Deleted(id1, d1)
      case (Updated(id1, d11, d12), Updated(id2, d21, d22)) if id1 == id2 && d12 == d21 => Updated(id1, d11, d22)
      case (Updated(id1, _, d12),   Deleted(id2, d2))       if id1 == id2 && d12 == d2  => Deleted(id1, d12)
      case (Deleted(id1, _),        Created(id2, d2))       if id1 == id2               => Created(id1, d2)
      case (x1, x2) =>
        throw new IllegalArgumentException(s"Conflict between $x1 and $x2")
    }
  }
}
