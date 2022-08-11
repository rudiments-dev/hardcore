package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.All

sealed trait Thing {}

trait Agent extends Thing {
  def read(where: Location): CRUD.O
  def ?(where: Location): CRUD.O = read(where)

  def find(where: Location, p: Predicate = All): CRUD.O
  def ?? (where: Location): CRUD.O = find(where, All)

  def !(where: Location): Link = this ? where match {
    case Readen(Node(_, leafs, _, _, _)) =>
      val selected = leafs.values.collect {
        case l: Link => l
      }.toSeq
      if (selected.nonEmpty) {
        Link(where, AnyOf(selected: _*))
      } else {
        throw new IllegalArgumentException(s"not only links in leafs")
      }
    case Readen(p: Predicate) => Link(where, p)
    case Readen(Data(p, _)) => Link(where, p)
    case other =>
      throw new IllegalArgumentException(s"don't know $other")
  }
}

final case class Link(where: Location, what: Predicate) extends Predicate {
  override def toString: String = "#" + where
}
final case class Declared(where: Location) extends Predicate {
  override def toString: String = "!" + where
}
final case class Data(what: Predicate, data: Any) extends Thing {
  override def toString: String = what match {
    case l: Link => l.toString + " {" + data.toString + "}"
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
  case object All extends Predicate
  case object Anything extends Predicate
}
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