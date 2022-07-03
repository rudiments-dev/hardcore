package dev.rudiments.hardcore.file

import dev.rudiments.hardcore._

import java.io.{File => JavaFile}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

class FileAgent(absolutePath: String) {
  val data: Memory = new Memory()
  val files: mutable.Map[Location, File] = mutable.Map.empty

  def read(where: Location): Out = where match {
    case Root => readFile(absolutePath, Folder)
    case id: ID => readFile(absolutePath + "/" + id.key.toString, files(id))
    case path: Path => readFile(absolutePath + "/" + path.toString, files(path))
  }

  def readFile(path: String, file: File): Out = file match {
    case Folder =>
      try {
        val file = new JavaFile(path)
        if(!file.isDirectory) throw new IllegalArgumentException(s"File $path should be directory")

        Readen(Data(Folder.typeOf,
          file.listFiles().toSeq.map {
            case f: JavaFile if f.isDirectory => ID(f.getName) -> Folder
            case f: JavaFile if f.isFile && TextFile.isTextFile(f.getName) => ID(f.getName) -> TextFile
            case f: JavaFile if f.isFile => ID(f.getName) -> UnknownFile
          }.toMap
        ))
      } catch {
        case e: Exception => FileError(e.getMessage)
      }

    case TextFile =>
      Using(Source.fromFile(path)) { f =>
        Readen(Data(TextFile.ofType, f.getLines().toSeq))
      }.getOrElse(FileError(s"Failed to read $path"))

    case UnknownFile =>
      Readen(Data(Binary, Files.readAllBytes(Paths.get(path))))
  }
}
