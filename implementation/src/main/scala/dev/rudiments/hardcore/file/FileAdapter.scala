package dev.rudiments.hardcore.file

import dev.rudiments.hardcore
import dev.rudiments.hardcore.{Agent, All, Create, Data, Find, Found, ID, In, Index, Memory, NoSkill, Out, RW, Read, Readen, ScalaTypes, Skill, Space, Thing, Type}

import java.io.{File => JavaFile}
import java.nio.file.{Files, Paths}
import scala.io.Source

sealed abstract class FileAdapter(val absolutePath: String) extends Agent(All, All) {}
case class Dir(
  override val absolutePath: String
)(implicit space: Space) extends FileAdapter(absolutePath) {
  val cache: Memory = new Memory(All, All)
  var totalRecursive = 0L
  private val textFiles = Seq("txt", "scala", "java", "gradle", "yml", "sql", "md", "conf", "xml", "http")
  private def isTextFile(name: String): Boolean = textFiles.exists(name.endsWith)

  private val enlist: Skill = RW (
    act = {
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
    commit = {
      case ReadenStructure(files) =>
        files.foreach {
          case (id, dir: Dir) =>
            cache.apply(Create(id, dir))
            totalRecursive += dir.totalRecursive
          case (id, file) =>
            cache.apply(Create(id, file))
            totalRecursive += 1
        }
        cache.skill.act(Find()) match {
          case Found(_, found: Map[ID, Thing]) => Data(
            Index(All, All),
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
    act = {
      case Read(id) =>
        cache.read(id) match {
          case r@Readen(_, _: Dir) =>
            r
          case r@Readen(i, txt: TextFile) =>
            if(txt.cache.isEmpty) {
              txt.skill.act(ReadFile) match {
                case readen: ReadenTextFile => FileReaden(i, readen)
                case out: Out => out
              }
            } else {
              r
            }
          case r@Readen(i, unk: UnknownFile) =>
            if(unk.cache.isEmpty) {
              unk.skill.act(ReadFile) match {
                case readen: ReadenBinaryFile => FileReaden(i, readen)
                case out: Out => out
              }
            } else {
              r
            }
        }
    },
    commit = {
      case Readen(_, file: FileAdapter) => file
      case FileReaden(id, readen) =>
        cache.read(id) match {
          case Readen(_, txt: TextFile) =>
            txt.skill.commit(readen)
            txt
          case Readen(_, unk: UnknownFile) =>
            unk.skill.commit(readen)
            unk
        }
    }
  )

  override val skill: RW = Skill(readContent, enlist, cache.skill)

  { // init
    this.apply(ReadStructure)
  }
}

case class TextFile(
  override val absolutePath: String
) extends FileAdapter(absolutePath) {
  var cache: Seq[String] = Seq.empty
  override val skill: RW = RW(
    act = {
      case ReadFile =>
        ReadenTextFile(Source.fromFile(absolutePath).getLines().toSeq)
    },
    commit = {
      case ReadenTextFile(commit) =>
        cache = commit
        new Data(hardcore.List(ScalaTypes.ScalaString), cache)
    }
  )
}

case class UnknownFile(
  override val absolutePath: String
) extends FileAdapter(absolutePath) {
  var cache: Array[Byte] = Array.empty
  override val skill: RW = RW(
    act = {
      case ReadFile =>
        ReadenBinaryFile(Files.readAllBytes(Paths.get(absolutePath)))
    },
    commit = {
      case ReadenBinaryFile(commit) =>
        cache = commit
        new Data(hardcore.List(ScalaTypes.ScalaString), cache)
    }
  )
}
