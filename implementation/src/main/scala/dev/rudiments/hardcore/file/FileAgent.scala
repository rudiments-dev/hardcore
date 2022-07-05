package dev.rudiments.hardcore.file

import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore._

import java.io.{File => JavaFile}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

class FileAgent(absolutePath: String) extends StrictLogging {
  val data: Memory = new Memory()
  val files: mutable.Map[Location, File] = mutable.Map.empty

  def read(where: Location): Out = where match {
    case Root => readFile(absolutePath, Folder)
    case id: ID => readFile(absolutePath + "/" + id.key.toString, files(id))
    case path: Path => readFile(absolutePath + "/" + path.toString, files(path))
  }

  def load(where: Location): Out = deltaTx(new Tx(data), where)

  def deltaTx(tx: Tx, where: Location): Out = (read(where), data.read(where)) match {
    case (Readen(file), NotExist) =>
      file match {
        case Data(Folder.typeOf, fs: Map[ID, File]) =>
          tx.remember(where, Created(file))
          fs.foreach { case (id, f) =>
            files += where / id -> f
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

  def readFile(path: String, file: File): Out = {
    val f = new JavaFile(path)
    if(!f.exists()) {
      logger.error("Can't open '{}' as File [{}]", path, file)
      NotExist
    } else {
      file match {
        case Folder =>
          try {
            if(!f.isDirectory) throw new IllegalArgumentException(s"File $path should be directory")

            Readen(Data(Folder.typeOf,
              f.listFiles().toSeq.map {
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
            Readen(Data(TextFile.typeOf, f.getLines().toSeq))
          }.getOrElse(FileError(s"Failed to read $path"))

        case UnknownFile =>
          try {
            Readen(Data(Binary, Files.readAllBytes(Paths.get(path))))
          } catch {
            case e: Exception => FileError(e.getMessage)
          }
      }
    }
  }
}
