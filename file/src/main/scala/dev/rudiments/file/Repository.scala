package dev.rudiments.file

import dev.rudiments.codecs.BytesCodec
import dev.rudiments.utils.SHA3

import java.io.File
import java.lang
import java.nio.ByteBuffer
import java.nio.file.{Path => FilePath}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class Repository(path: FilePath) {
  private val dir = path.toAbsolutePath

  val state: mutable.Map[Path, FS] = mutable.Map.empty
  val log: mutable.Buffer[Commit] = mutable.Buffer.empty
  
  val ignored: Set[Path] = Set(Seq(".git"), Seq(".gradle"), Seq(".idea"))

  def read(): Unit = {
    given tx: Tx = new Tx(state.toMap)
    val file = dir.toFile
    val readen: FS = if(file.isFile) {
      readFileUnsafe(file)
    } else if(file.isDirectory) {
      readDirUnsafe(file, Seq.empty)
    } else {
      throw new IllegalArgumentException("Not a file or dir")
    }
    tx.put(Seq.empty, readen)

    val commit = tx.makeCommit
    if(commit.changes.nonEmpty) {
      log.append(commit)
      state ++= commit.changed
    }
  }

  private def readDirUnsafe(file: File, prefix: Path)(using tx: Tx): Dir = {
    val content = file.listFiles().toList.sortBy(_.getName)
      .filterNot(f => ignored.contains(prefix :+ f.getName))
      .map {
        case f if f.isFile => f.getName -> readFileUnsafe(f)
        case f if f.isDirectory => f.getName -> readDirUnsafe(f, prefix :+ f.getName)
      }

    content.foreach((k, v) => tx.put(prefix :+ k, v))

    val (dirs, blobs) = content.partitionMap {
      case (s, _: Dir) => Left(s)
      case (s, _: Binary) => Right(s)
    }
    Dir(dirs, blobs)
  }

  private def readFileUnsafe(file: File): Binary = {
    Binary(java.nio.file.Files.readAllBytes(file.toPath))
  }
}

type Path = Seq[String]

sealed trait FS {
  def about: About
}

import dev.rudiments.file.About.Type
case class Dir(directions: Path, files: Path) extends FS {
  lazy val data: Array[Byte] = BytesCodec.encodeStrings(directions) ++ BytesCodec.encodeStrings(files)

  override def about: About = About(Type.Dir, data.length, SHA3(data))
}
case class Binary(data: Seq[Byte]) extends FS {
  override def about: About = About(Type.File, data.size, SHA3(data.toArray[Byte]))
}
object Binary {
  def apply(data: Array[Byte]): Binary = new Binary(ArraySeq.unsafeWrapArray(data))
}

case object NotExist extends FS {
  override def about: About = About(Type.File, 0, SHA3.empty)
}