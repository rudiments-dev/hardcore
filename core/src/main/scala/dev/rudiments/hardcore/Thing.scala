package dev.rudiments.hardcore

sealed trait Thing {}

trait Agent extends Thing {
  def read(where: Location): CRUD.O
  def ?(where: Location): CRUD.O = read(where)

  def report(q: Query): CRUD.O
  def ?? (where: Location): CRUD.O = read(where) match {
    case Readen(n: Node) => n.report(Find(All))
    case other => Conflict(other, Find(All))
  }
  def ?* (where: Location): CRUD.O = read(where) match {
    case Readen(n: Node) => n.report(LookFor(All))
    case other => Conflict(other, LookFor(All))
  }
  def ?** (where: Location): CRUD.O = read(where) match {
    case Readen(n: Node) => n.report(Dump(All))
    case other => Conflict(other, Dump(All))
  }

  def !(where: Location): Link = this ? where match {
    case Readen(Node(_, _, _, relations, _, _)) => relations.get(ID("Partners")) match {
      case Some(related) => Link(where, AnyOf(related.map(l => this ! l): _*))
    }
    case Readen(p: Predicate) => Link(where, p)
    case Readen(Data(p, _)) => Link(where, p)
    case other =>
      throw new IllegalArgumentException(s"don't know $other")
  }
}

final case class Link(where: Location, what: Predicate) extends Predicate {
  override def toString: String = "#" + where

  def data(values: Any*): Data = Data(this, values.toSeq)
}
final case class Declared(where: Location) extends Predicate {
  override def toString: String = "!" + where
}
final case class Data(what: Predicate, data: Any) extends Thing {
  override def toString: String = what match {
    case l: Link => l.toString + " {" + data.toString + "}"
    case t: Type => data match {
      case s: Seq[Any] => t.fields.map(_.name).zip(s).mkString("{", ",", "}")
    }
    case Binary =>
      data match {
        case Nothing => "Nothing"
        case arr: Seq[Byte] => "binary: " + arr.mkString("[", " ", "]")
      }
    case _ => super.toString
  }
}
object Data {
  val empty = Data(Nothing, Nothing)
}

sealed trait Predicate extends Thing {}
object Predicate {
  case object Anything extends Predicate
}
case object All extends Predicate

final case class Type(fields: Field*) extends Predicate {
  override def toString: String = fields.mkString("{", ",", "}")

  def data(values: Any*): Data = Data(this, values.toSeq)
}
final case class Field(name: String, of: Predicate) {
  override def toString: String = name + ":" + of.toString
} //TODO snapshot & restore for Memory[Text, Field] -> Type -> Memory[Text, Field]

final case class Enlist(item: Predicate) extends Predicate {
  override def toString: String = "[" + item.toString + "]"
}
final case class Index(of: Predicate, over: Predicate) extends Predicate {
  override def toString: String = "{" + of.toString + "->" + over.toString + "}"
}
final case class AnyOf(p: Predicate*) extends Predicate {
  override def toString: String = p.mkString("*(", ",", ")")
} // Sum-Type

sealed trait Plain extends Predicate {}
final case class Text(maxSize: Int) extends Plain
final case class Number(from: AnyVal, upTo: AnyVal) extends Plain //TODO replace with more strict version
case object Bool extends Plain {} // funny thing - in scala we can't extend object, so, or 'AnyBool' under trait, or no True and False under Bool object
case object Binary extends Plain {} // Array[Byte]

sealed trait Temporal extends Plain {}
case object Date extends Temporal
case object Time extends Temporal
case object Timestamp extends Temporal


sealed trait Abstraction extends Thing {}
case object Nothing extends Predicate {}

trait Message extends Thing {} //TODO separate CRUD+ from Message
trait In extends Message {}
trait Out extends Message {}
trait Command extends In {}
trait Event extends Out {}
trait Query extends In {}
trait Report extends Out {}
trait Error extends Out {}

sealed trait Location extends Thing {
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