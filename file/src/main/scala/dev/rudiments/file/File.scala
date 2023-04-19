package dev.rudiments.file

import dev.rudiments.utils.SHA3

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets.UTF_8


sealed trait Files(val header: Header) {}
object Files {
  def apply(path: Path): Files = {
    val file = path.toFile
    if(file.isDirectory) {
      Many.apply(file.getName, file.listFiles().toList)
    } else {
      One.apply(file.getName, java.nio.file.Files.readAllBytes(file.toPath))
    }
  }
}

case class One(override val header: Header, content: Barr) extends Files(header) {
  override def equals(obj: Any): Boolean = obj match
    case One(h, c) => this.header.equals(h) && this.content.sameElements(c)
    case _ => false
}
object One {
  def apply(path: Path): One = {
    val file = path.toFile
    if(file.isFile) {
      One.apply(file.getName, java.nio.file.Files.readAllBytes(file.toPath))
    } else {
      throw new IllegalArgumentException("Not a file")
    }
  }

  def apply(name: String, content: Barr): One = new One(Header(name, Header.Type.File, content.length, SHA3(content)), content)
}

case class Many(self: Header, items: Seq[Header]) extends Files(self) {}
object Many {
  def apply(path: Path): Many = {
    val file = path.toFile
    if (file.isDirectory) {
      Many.apply(file.getName, file.listFiles().toList)
    } else {
      throw new IllegalArgumentException("Not a directory")
    }
  }

  def apply(name: String, files: List[File]): Many = {
    val readen = files.map(f => Files(f.toPath).header)
    val size = readen.foldLeft(0){ (acc, i) => acc + i.size }
    val bytes = readen.foldLeft(Array.empty[Byte]){ (acc, i) => acc ++ i.toByteArray}
    new Many(
      new Header(name, Header.Type.Dir, size, SHA3(bytes)),
      readen
    )
  }
}

