package dev.rudiments.hardcore.file

import dev.rudiments.hardcore._

import scala.collection.Seq

sealed trait File {}
object File {
  val folder: Link = Link(ID("Folder"), Nothing)
  val textFile: Link = Link(ID("TextFile"), Nothing)
  val unknownFile: Link = Link(ID("UnknownFile"), Nothing)

  val file: Predicate = AnyOf(folder, textFile, unknownFile)
}

case object Folder extends File {
  val typeOf: Predicate = Index(Text(Int.MaxValue), File.file)
}

case object TextFile extends File {
  val typeOf: Predicate = Enlist(Text(Int.MaxValue))

  val textFileExtensions: Seq[String] = Seq(".txt", ".scala", ".java", ".gradle", ".yml", ".sql", ".md", ".conf", ".xml", ".http", ".json")
  def isTextFile(name: String): Boolean = textFileExtensions.exists(name.endsWith)
}

case object UnknownFile extends File {
  val empty: Data = Data(Binary, Nothing)
}