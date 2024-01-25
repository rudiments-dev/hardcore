package dev.rudiments.codecs

import scala.compiletime.{constValue, erasedValue, error, summonFrom}
import scala.deriving.Mirror

case class MirrorInfo[A](
  name: String,
  fields: Seq[(String, MirrorInfo[_])]
)

object MirrorInfo {
  given intInfo: MirrorInfo[Int] = MirrorInfo("Int", Seq.empty)
  given strInfo: MirrorInfo[String] = MirrorInfo("String", Seq.empty)

  inline final def summonInfo[A]: MirrorInfo[A] = summonFrom {
    case i: MirrorInfo[A] => i
    case _: Mirror.Of[A] => apply[A]
  }

  inline final def fieldsInfo[A <: Tuple]: List[MirrorInfo[_]] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInfo[t] :: fieldsInfo[ts]
  }

  inline final def summonLabelsRec[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]
  }

  inline final def apply[A](using A: Mirror.Of[A]): MirrorInfo[A] = {
    val name = constValue[A.MirroredLabel].asInstanceOf[String]
    val labels = summonLabelsRec[A.MirroredElemLabels]
    val fields = fieldsInfo[A.MirroredElemTypes]

    MirrorInfo(name, labels.zip(fields))
  }
}
