package dev.rudiments.hardcore

import scala.reflect.ClassTag

//ADT: Ref -> ID(1, 2, 3, path), Data + ADT: Thing: Abstract, Data, Type, Plain..., both sealed

sealed trait Ref[A] {}

sealed trait Thing {}

final case class Abstract(name: String, fields: Map[String, Field]) extends Store[Field] with Thing

final case class Data[A: ClassTag](fields: Seq[Any]) extends Store[Any] with Thing with Ref[A] { // Store[What??]
  def toScala: A =
    implicitly[ClassTag[A]].runtimeClass.getConstructors()(0).newInstance(fields: _*).asInstanceOf[A]
}

final case class Type(name: String, fields: Map[String, Field]) extends Store[Field] with Thing


sealed abstract class ID[A: ClassTag] extends Ref[A] {
  val is: String = implicitly[ClassTag[A]].runtimeClass.getSimpleName
  def toPair: (String, ID[A]) = is -> this

  def /[B: ClassTag](path: Path[B]): Path[B] = Path(path.to, this +: path.via: _*)
  def /[B: ClassTag](another: ID[B]): Path[B] = Path(another, this)
}
object ID {
  def apply[A: ClassTag, K](key: K): ID[A] = ID1(key)
  def apply[A: ClassTag, K1, K2](key1: K1, key2: K2): ID[A] = ID2(key1, key2)
  def apply[A: ClassTag, K1, K2, K3](key1: K1, key2: K2, key3: K3): ID[A] = ID3(key1, key2, key3)
}

final case class ID1[A: ClassTag, K](key: K) extends ID[A]
final case class ID2[A: ClassTag, K1, K2](key1: K1, key2: K2) extends ID[A]
final case class ID3[A: ClassTag, K1, K2, K3](key1: K1, key2: K2, key3: K3) extends ID[A]

final case class Path[A: ClassTag](to: ID[A], via: ID[_]*) extends ID[A] {
  override def /[B: ClassTag](id: ID[B]): Path[B] = Path(id, this.via :+ this.to : _*)

  def toMap: Map[String, ID[_]] = (via :+ to).map(_.toPair).toMap
}


final case class Field(name: String, thing: Thing, required: Boolean) extends ADT