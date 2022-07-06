package dev.rudiments.hardcore.file

import dev.rudiments.hardcore._

import java.io.{File => JavaFile}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

class FileAgent(absolutePath: String) {
  def read(where: Location): Out = where match {
    case Root => readFile(absolutePath)
    case id: ID => readFile(absolutePath + "/" + id.key.toString)
    case path: Path => readFile(absolutePath + "/" + path.toString)
  }

  def load(where: Location, into: Memory): Out = deltaTx(new Tx(into), where)

  def deltaTx(tx: Tx, where: Location): Out = (read(where), tx.read(where)) match {
    case (Readen(file), NotExist) =>
      file match {
        case Data(Folder.typeOf, fs: Map[ID, File]) =>
          tx.remember(where, Created(file))
          fs.foreach { case (id, f) =>
            deltaTx(tx, where / id)
          }
          tx.>>
        case Data(TextFile.typeOf, _) => tx.remember(where, Created(file))
        case Data(_, _) => tx.remember(where, Created(Data(Binary, Nothing)))
      }
    case (Readen(file), Readen(found)) =>
      (file, found) match {
        case (Data(Folder.typeOf, incoming: Map[ID, File]), Data(Folder.typeOf, existing: Map[ID, File])) =>
          val allKeys = existing.keys ++ incoming.keys
          allKeys.foreach { k => deltaTx(tx, where / k) }
          tx.>>
        case (_, _) => tx.remember(where, Updated(found, file))
      }
    case (NotExist, Readen(found)) => Deleted(found)
    case (NotExist, NotExist) => NotExist
    case (that, other) => Conflict(that, other)
  }

  def readFile(path: String): Out = {
    val f = new JavaFile(path)
    if(!f.exists()) {
      NotExist
    } else {
      if(f.isDirectory) {
        Readen(Data(Folder.typeOf,
          f.listFiles().toSeq.map {
            case f: JavaFile if f.isDirectory => ID(f.getName) -> File.folder
            case f: JavaFile if f.isFile && TextFile.isTextFile(f.getName) => ID(f.getName) -> File.textFile
            case f: JavaFile if f.isFile => ID(f.getName) -> File.unknownFile
          }.toMap
        ))
      } else if (f.isFile) {
        if(TextFile.isTextFile(f.getName)) {
          Using(Source.fromFile(path)) { f =>
            Readen(Data(TextFile.typeOf, f.getLines().toSeq))
          }.getOrElse(FileError(s"Failed to read $path"))
        } else {
          Readen(Data(Binary, Files.readAllBytes(Paths.get(path))))
        }
      } else {
        FileError(s"Unknown file ${f.getAbsolutePath}")
      }
    }
  }
}
