package dev.rudiments.hardcore

sealed trait Thing {}
sealed trait Predicate extends Thing {}
object Anything extends Predicate
object Nothing extends Predicate
sealed trait Plain extends Predicate {}
object Bool extends Plain
object Number extends Plain
object Text extends Plain
sealed trait Temporal extends Plain {}
object Time extends Temporal
object Date extends Temporal
object Timestamp extends Temporal

final case class Many(of: Predicate) extends Predicate {}
final case class Index(over: Predicate, of: Predicate) extends Predicate {}

final case class PredicateRef(to: Location[String]) extends Predicate {}
final case class ThingRef(to: Location[String]) extends Thing {}

final case class Type(fields: Seq[(String, Field)]) extends Predicate {

  def validate(data: Any): Either[Exception, Any] = ???

  def isAbstract: Boolean = fields.exists {
    case (_, Field.Declared(_, _)) => true
    case (_, Field.Abstract(_, _)) => true
    case _ => false
  }

  def nonAbstract: Boolean = !isAbstract

  def make(data: Any): Data = validate(data) match
    case Right(v) => Data(this, v)
    case Left(err) => throw err
}

enum Field extends Thing {
  case Value(of: Predicate, required: Boolean)
  case Abstract(of: Predicate, required: Boolean)
  case Method(in: Predicate, out: Predicate, f: Any => Any)
  case Declared(in: Predicate, out: Predicate)
}

enum TypeEdges extends Thing {
  case Is, Has, Extends, Realizes
} 

//TODO where to put array predicates?
enum ManyPredicates extends Predicate {
  case Unique, Sequence
  case Ordered(direction: OrderDirection)
  case MaxSize(size: Long)
  case MinSize(size: Long)
  case ValueShould(be: Predicate)
}

enum OrderDirection extends Thing:
  case Asc, Desc

final case class Data(of: Predicate, values: Any) extends Thing {}