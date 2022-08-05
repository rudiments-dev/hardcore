package dev.rudiments.hardcore.file

import dev.rudiments.hardcore.{Command, Data, Error, Event, ID}

sealed trait FileSystemIO {}
case object ClearCache extends Command with FileSystemIO

sealed trait DirIO extends FileSystemIO {}
case object ReadStructure extends Command with DirIO
case class ReadenStructure(files: Map[ID, File]) extends Event with DirIO


sealed trait FileIO extends FileSystemIO {}
case object ReadFile extends Command with FileIO
case class ReadenTextFile(lines: Data) extends Event with FileIO
case class ReadenBinaryFile(content: Data) extends Event with FileIO

case class WriteTextFile(lines: Data) extends Command with FileIO
case class WrittenTextFile(content: Data) extends Event with FileIO


case class FileError(message: String) extends Error with FileSystemIO