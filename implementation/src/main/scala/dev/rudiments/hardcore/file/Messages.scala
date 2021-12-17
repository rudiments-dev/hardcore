package dev.rudiments.hardcore.file

import dev.rudiments.hardcore.{Command, Error, Event, ID}

sealed trait FileSystemIO {}
case object ClearCache extends Command with FileSystemIO

sealed trait DirIO extends FileSystemIO {}
case object ReadStructure extends Command with DirIO
case object ReadRecursiveStructure extends Command with DirIO
case class ReadenStructure(files: Map[ID, FileAdapter]) extends Event with DirIO
case class ReadenRecursiveStructure() extends Event with DirIO
case class ClearedCache() extends Event with DirIO


sealed trait FileIO extends FileSystemIO {}
case object ReadFile extends Command with FileIO
case class ReadenTextFile(content: Seq[String]) extends Event with FileIO
case class ReadenBinaryFile(content: Array[Byte]) extends Event with FileIO
case class FileReaden(id: ID, evt: Event with FileIO) extends Event with FileIO


case class FileError(message: String) extends Error with FileSystemIO