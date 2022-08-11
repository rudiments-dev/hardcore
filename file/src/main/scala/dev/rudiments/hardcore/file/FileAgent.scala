package dev.rudiments.hardcore.file

import dev.rudiments.hardcore.CRUD.Evt
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder

import java.io.{FileWriter, File => JavaFile}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

class FileAgent(absolutePath: String) {
  def read(where: Location): Out = where match {
    case Root => readFile(absolutePath)
    case id: ID => readFile(absolutePath + "/" + id.key.toString)
    case path: Path => readFile(absolutePath + "/" + path.toString)
    case other => throw new IllegalArgumentException(s"Unsupported: $other")
  }

  def compose(where: Location): Thing = {
    read(where) match {
      case Readen(d@Data(Folder.typeOf, folders: Map[ID, File])) =>
        val loaded = folders.map { case (k, _) => k -> compose(where / k) }
        val errors = loaded.collect { case p@(_, _: Error) => p }

        if(errors.isEmpty) {
          val branches: Map[ID, Node] = loaded.collect { case (id, m: Node) => id -> m }
          val data: Map[ID, Thing] = loaded.collect {
            case (_, _: Out) => None //TODO more invalid options
            case (_, _: Node) => None
            case p@(_, _) => Some(p)
          }.flatten.toMap
          Node(
            d,
            data,
            branches
          )
        } else {
          MultiError(errors.asInstanceOf[Map[Location, Error]])
        }
      case Readen(d: Data) => d
      case other => throw new IllegalStateException(s"Where '$other' coming from?")
    }
  }

  def reconsFor(mem: Node): Out = {
    compose(Root) match {
      case err: Error => err
      case out: Out => out
      case m: Node =>
        val compared = mem.compareWith(m)
        val changes = compared.collect { case (l, evt: Evt) => l -> evt }
        val errors = changes.collect { case (l, err: Error) => l -> err }
        if (errors.isEmpty && changes.nonEmpty) {
          if(changes.size == 1) {
            if(changes.keys.head == Root) {
              changes(Root)
            } else {
              Prepared(Commit(changes))
            }
          } else {
            Prepared(Commit(changes))
          }
        } else if (changes.isEmpty) {
          Identical
        } else {
          MultiError(errors)
        }
      case t: Thing =>
        (t, mem.self) match {
          case (d, Nothing) => Created(d)
          case (Nothing, Nothing) => Readen(Nothing)
          case (d, s) if d != s => Updated(s, d)
          case (Nothing, s) => Deleted(s)
        }
    }
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
          Readen(Data(Binary, Files.readAllBytes(Paths.get(path)).toSeq))
        }
      } else {
        FileError(s"Unknown file ${f.getAbsolutePath}")
      }
    }
  }

  def writeFileFromNode(node: Node, where: Location): Out = { //TODO File events?
    if(where == Root) { //will return AlreadyExist if directory already exist
      mkDir(absolutePath)
    } else {
      mkDir(absolutePath + "/" + where)
    }

    val branches = node.branches.map { case (id, n) =>
      //TODO if all leafs and branches Deleted ?
      mkDir(absolutePath + "/" + (where / id).toString)
      id -> writeFileFromNode(n, where / id)
    }

    val leafs = node.leafs.map { //TODO more checks on update and delete?
      case (id, Created(data)) => id -> writeFile((where / id).toString, data)
      case (id, Updated(_, data)) => id -> writeFile((where / id).toString, data)
      case (id, Deleted(_)) => id -> deleteFile((where / id).toString)
      case (id, c: Commit) => id -> writeFile((where / id).toString, c)
      case (id, data: Data) => id -> writeFile((where / id).toString, data)
      case (id, Nothing) => id -> writeFile((where / id).toString, Nothing)
      case (id, _) =>
        id -> NotImplemented
    }

    val errors = leafs ++ branches filter {
      case (_, _: Error) => true
      case _ => false
    }

    if(errors.nonEmpty) {
      MultiError(errors.toMap)
    } else {
      WrittenTextFile(Data.empty)
    }
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
        case Data(Binary, Nothing) =>
          f.createNewFile()
          Created(Nothing)
        case d@Data(Binary, _: Seq[Byte]) =>
          f.createNewFile()
          Created(d)
        case cmt: Commit =>
          f.createNewFile()
          val writer = new FileWriter(f, Charset.defaultCharset())
          try {
            writer.write(ThingEncoder.encodeOut(Prepared(cmt)).toString())
            Created(cmt)
          } finally {
            writer.close()
          }
        case Nothing =>
          f.createNewFile()
          Created(Nothing)
        case _ =>
          NotImplemented
      }
    }

    val f = new JavaFile(absolutePath + "/" + path)
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
    val f = new JavaFile(absolutePath + "/" + path)
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
