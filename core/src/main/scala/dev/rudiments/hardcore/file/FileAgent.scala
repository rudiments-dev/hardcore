package dev.rudiments.hardcore.file

import dev.rudiments.hardcore._

import java.io.{FileWriter, File => JavaFile}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

class FileAgent(absolutePath: String, mount: Location) {
  def read(where: Location): Out = where match {
    case Root => readFile(absolutePath)
    case id: ID => readFile(absolutePath + "/" + id.key.toString)
    case path: Path => readFile(absolutePath + "/" + path.toString)
    case other => throw new IllegalArgumentException(s"Unsupported: $other")
  }

  def load(where: Location, into: Context): Out = {
    val tx = new Tx(into)
    readFileIntoTx(tx, where)
    tx.>>
  }

  def readFileIntoTx(tx: Tx, where: Location): Unit = (read(where), tx.read(mount / where)) match {
    case (Readen(file), NotExist) =>
      file match {
        case Data(Folder.typeOf, fs: Map[ID, File]) =>
          tx.remember(mount / where, Created(file))
          fs.foreach { case (id, _) => readFileIntoTx(tx, where / id) }
        case Data(TextFile.typeOf, _) => tx.remember(mount / where, Created(file))
        case Data(Binary, _) => tx.remember(mount / where, Created(Data(Binary, Nothing)))
        case other => ???
      }
    case (Readen(file), Readen(found)) =>
      (file, found) match {
        case (d1@Data(Folder.typeOf, incoming: Map[ID, File]), d2@Data(Folder.typeOf, existing: Map[ID, File])) =>
          val allKeys = existing.keys ++ incoming.keys
          if(d1 != d2) {
            tx.remember(mount / where, Updated(d1, d2))
          }
          allKeys.foreach { k => readFileIntoTx(tx, where / k) }
        case (t1@Data(TextFile.typeOf, _), t2@Data(TextFile.typeOf, _)) => if(t1 != t2){
          tx.remember(mount / where, Updated(t1, t2))
        }
        case (Data(Binary, _), Data(Binary, _)) => // do nothing with binaries
        case (_, _) => ???
      }
    case (NotExist, Readen(found)) => tx.remember(mount / where, Deleted(found))
    case (NotExist, NotExist) => NotExist
    case (that, other) => tx.remember(mount / where, Conflict(that, other))
  }

  def readFile(path: String): Out = {
    val f = new JavaFile(path)
    if(!f.exists()) {
      NotExist
    } else {
      if(f.isDirectory) {
        Readen(Data(Folder.typeOf,
          f.listFiles().toSeq.collect {
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

  def writeFileFromTx(tx: Tx, where: Location): Out = {
    Memory.fromMap(tx.last.toMap).seek(mount / where) match {
      case Some(node) => writeFileFromNode(node, where)
      case None => NotExist
    }
  }

  def writeFileFromNode(node: Memory, where: Location): Out = { //TODO File events?
    mkDir(absolutePath) //will return AlreadyExist if directory already exist

    val leafs = node.leafs.map { //TODO more checks on update and delete?
      case (id, Created(data)) => id -> writeFile(absolutePath + "/" + (where / id).toString, data)
      case (id, Updated(_, data)) => id -> writeFile(absolutePath + "/" + (where / id).toString, data)
      case (id, Deleted(_)) => id -> deleteFile(absolutePath + "/" + (where / id).toString)
      case (id, _) => id -> NotImplemented
    }

    val branches = node.branches.map { case(id, n) =>
      //TODO if all leafs and branches Deleted ?
      val out = mkDir(absolutePath + "/" + (where / id).toString)
      writeFileFromNode(n, where / id)
    }

    WrittenTextFile(Data.empty)
  }

  def mkDir(path: String): Out = {
    val f = new JavaFile(path)
    try {
      if(!f.exists()) {
        f.mkdir()
        Created(Data.empty)
      } else {
        AlreadyExist(Data.empty) //TODO read dir for info?
      }
    } catch {
      case e: Exception => FileError(e.getMessage)
    }
  }

  def writeFile(path: String, content: Any): Out = {
    def wf(f: JavaFile): Out = {
      content match {
        case d@Data(TextFile.typeOf, content: Seq[String]) =>
          f.createNewFile()
          val writer = new FileWriter(f, Charset.defaultCharset())
          try {
            content.foreach { s =>
              writer.write(s)
              writer.write("\n")
            }
            Created(d)
          } finally {
            writer.close()
          }
        case _ => NotImplemented
      }
    }

    val f = new JavaFile(path)
    try {
      if(!f.exists()) {
        wf(f)
      } else {
        f.delete()
        wf(f)
      }
    } catch {
      case e: Exception => FileError(e.getMessage)
    }
  }

  def deleteFile(path: String): Out = {
    val f = new JavaFile(path)
    try {
      if(!f.exists()) {
        NotExist
      } else {
        f.delete()
        Deleted(Data.empty)
      }
    } catch {
      case e: Exception => FileError(e.getMessage)
    }
  }
}
