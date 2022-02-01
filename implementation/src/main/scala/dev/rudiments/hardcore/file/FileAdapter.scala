package dev.rudiments.hardcore.file

import dev.rudiments.hardcore
import dev.rudiments.hardcore.ScalaTypes.ScalaString
import dev.rudiments.hardcore.{Agent, All, Create, Data, Find, Found, ID, In, Index, Memory, NoSkill, Out, Path, RW, Read, Readen, ScalaTypes, Skill, Space, Thing, Type}

import java.io.{File => JavaFile}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

sealed abstract class FileAdapter(val absolutePath: String) extends Agent(All, All) {}
case class Dir(
  override val absolutePath: String
)(implicit space: Space) extends FileAdapter(absolutePath) {
  val cache: Memory = new Memory(ScalaString, Path("types/FileAdapter").ref)
  var totalRecursive = 0L
  private def isTextFile(name: String): Boolean = Dir.textFiles.exists(name.endsWith)

  private val enlist: Skill = RW (
    query = {
      case ReadStructure =>
        try {
          val file = new JavaFile(absolutePath)
          if(!file.isDirectory) throw new IllegalArgumentException(s"File $absolutePath should be directory")

          ReadenStructure(
            file.listFiles().toSeq.map {
              case f: JavaFile if f.isDirectory => ID(f.getName) -> Dir(f.getAbsolutePath)
              case f: JavaFile if f.isFile && isTextFile(f.getName) => ID(f.getName) -> TextFile(f.getAbsolutePath)
              case f: JavaFile if f.isFile => ID(f.getName) -> UnknownFile(f.getAbsolutePath)
            }.toMap
          )
        } catch {
          case e: Exception => FileError(e.getMessage)
        }
    },
    write = {
      case ReadenStructure(files) =>
        files.foreach {
          case (id, dir: Dir) =>
            cache << Create(id, dir)
            totalRecursive += dir.totalRecursive
          case (id, file) =>
            cache << Create(id, file)
            file << ReadFile
            totalRecursive += 1
        }
        cache.skill.query(Find()) match {
          case Found(_, found: Map[ID, Thing]) => Data(
            Index(ScalaString, All),
            found.collect {
              case (id, file: Dir) => id -> Data.build[Dir](file.absolutePath)
              case (id, file: TextFile) => id -> Data.build[TextFile](file.absolutePath)
              case (id, file: UnknownFile) => id -> Data.build[UnknownFile](file.absolutePath)
            }
          )
        }
    }
  )

  private val readContent: RW = RW (
    query = {
      case Read(id) =>
        cache.read(id) match {
          case r@Readen(_, _: Dir) =>
            r
          case r@Readen(i, txt: TextFile) =>
            if(txt.cache.isEmpty) {
              txt.skill.query(ReadFile) match {
                case readen: ReadenTextFile => FileReaden(i, readen)
                case out: Out => out
              }
            } else {
              r
            }
          case r@Readen(i, unk: UnknownFile) =>
            if(unk.cache.isEmpty) {
              unk.skill.query(ReadFile) match {
                case readen: ReadenBinaryFile => FileReaden(i, readen)
                case out: Out => out
              }
            } else {
              r
            }
        }
    },
    write = {
      case Readen(_, file: FileAdapter) => file
      case FileReaden(id, readen) =>
        cache.read(id) match {
          case Readen(_, txt: TextFile) =>
            txt.skill.write(readen)
            txt
          case Readen(_, unk: UnknownFile) =>
            unk.skill.write(readen)
            unk
        }
    }
  )

  override val skill: RW = Skill(readContent, enlist, cache.skill)

  { // init
    this << ReadStructure
  }
}

object Dir {
  val textFiles = Seq("txt", "scala", "java", "gradle", "yml", "sql", "md", "conf", "xml", "http")
}

case class TextFile(
  override val absolutePath: String
) extends FileAdapter(absolutePath) {
  var cache: Seq[String] = Seq.empty
  val readFile: RW = RW(
    query = {
      case ReadFile =>
        Using(Source.fromFile(absolutePath)) { f =>
          ReadenTextFile(f.getLines().toSeq)
        }.getOrElse(FileError(s"Failed to read $absolutePath"))
    },
    write = {
      case ReadenTextFile(commit) =>
        cache = commit
        new Data(hardcore.List(ScalaTypes.ScalaString), cache)
    }
  )

  val writeFile: RW = RW(
    query = {
      case WriteTextFile(content) => WrittenTextFile(content)
    },
    write = {
      case WrittenTextFile(content) =>
        cache = content
        Files.write(
          Paths.get(absolutePath),
          content.mkString("\n").getBytes(StandardCharsets.UTF_8)
        )
        new Data(hardcore.List(ScalaTypes.ScalaString), cache)
    }
  )

  override val skill: RW = Skill(readFile, writeFile)
}

case class UnknownFile(
  override val absolutePath: String
) extends FileAdapter(absolutePath) {
  var cache: Array[Byte] = Array.empty
  override val skill: RW = RW(
    query = {
      case ReadFile =>
        ReadenBinaryFile(
          Files.readAllBytes(Paths.get(absolutePath))
        )
    },
    write = {
      case ReadenBinaryFile(commit) =>
        cache = commit
        new Data(hardcore.List(ScalaTypes.ScalaString), cache)
    }
  )
}
