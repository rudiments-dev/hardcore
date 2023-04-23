package dev.rudiments.file

import dev.rudiments.utils.BytesCodec
import dev.rudiments.utils.SHA3

import java.io.File
import java.lang
import java.nio.ByteBuffer
import java.nio.file.Path
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class Repository(path: Path) {
  private val dir = path.toAbsolutePath

  val state: mutable.Map[Seq[String], FileData] = mutable.Map.empty

  def read(): Unit = {
    val file = dir.toFile
    val readen: FileData = if(file.isFile) {
      readFileUnsafe(file)
    } else if(file.isDirectory) {
      readDirUnsafe(file, Seq.empty)
    } else {
      throw new IllegalArgumentException("Not a file or dir")
    }
    state.put(Seq.empty, readen)
  }

  private def readDirUnsafe(file: File, prefix: Seq[String]): Dir = {
    val content = file.listFiles().toList.sortBy(_.getName).map {
      case f if f.isFile => f.getName -> readFileUnsafe(f)
      case f if f.isDirectory => f.getName -> readDirUnsafe(f, prefix :+ f.getName)
    }

    content.foreach((k, v) => state.put(prefix :+ k, v))

    val (dirs, blobs) = content.partitionMap {
      case (s, _: Dir) => Left(s)
      case (s, _: Blob) => Right(s)
    }
    Dir(dirs, blobs)
  }

  private def readFileUnsafe(file: File): Blob = {
    Blob(java.nio.file.Files.readAllBytes(file.toPath))
  }
}

sealed trait FileData {
  def about: About
}

import dev.rudiments.file.About.Type
case class Dir(directions: Seq[String], files: Seq[String]) extends FileData {
  lazy val data: Array[Byte] = BytesCodec.encodeStrings(directions) ++ BytesCodec.encodeStrings(files)

  override def about: About = About(Type.Dir, data.length, SHA3(data))
}
case class Blob(data: Seq[Byte]) extends FileData {
  override def about: About = About(Type.File, data.size, SHA3(data.toArray[Byte]))
}
object Blob {
  def apply(data: Array[Byte]): Blob = new Blob(ArraySeq.unsafeWrapArray(data))
}
