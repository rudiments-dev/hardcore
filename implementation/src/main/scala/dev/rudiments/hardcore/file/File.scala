package dev.rudiments.hardcore.file

import dev.rudiments.hardcore._

import scala.collection.Seq

sealed trait File {}
object File {
  val typeOf: Predicate = Type(Field("absolutePath", Text(Int.MaxValue)))
}

case object Folder extends File {
  val typeOf: Predicate = Index(Text(Int.MaxValue), File.typeOf)
}

case object TextFile extends File {
  val ofType: Predicate = Enlist(Text(Int.MaxValue))
  val empty: Data = Data(ofType, Nothing)

  val textFileExtensions: Seq[String] = Seq(".txt", ".scala", ".java", ".gradle", ".yml", ".sql", ".md", ".conf", ".xml", ".http")
  def isTextFile(name: String): Boolean = textFileExtensions.exists(name.endsWith)
}

case object UnknownFile extends File {
  val empty: Data = Data(Binary, Nothing)
}