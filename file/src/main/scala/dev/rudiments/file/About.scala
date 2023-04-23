package dev.rudiments.file

import dev.rudiments.utils.SHA3

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.ArraySeq


case class About(
  fileType: About.Type,
  size: Int,
  checksum: SHA3
) {
  def toByteArray: Array[Byte] = {
    ByteBuffer.allocate(1 + 32 + 4)
      .put(checksum.asArray)
      .put((fileType.ordinal + 1).toByte)
      .putInt(size)
      .array()
  }
}

object About {
  def apply(data: Array[Byte]): About = {
    val buff = ByteBuffer.wrap(data)
    val hash = new Array[Byte](32)
    buff.get(hash)
    val fileType = About.Type(buff.get())
    val size = buff.getInt
    new About(fileType, size, new SHA3(ArraySeq.unsafeWrapArray(hash)))
  }

  enum Type:
    case File, Dir;

  object Type {
    def apply(b: Byte): Type = b match
      case 1 => File
      case 2 => Dir
      case _ => throw new IllegalArgumentException(s"Not supported type from header: $b")
  }
}
