package dev.rudiments.file

import dev.rudiments.utils.SHA3

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8


type Barr = Array[Byte]

case class Header(
  name: String,
  fileType: Header.Type,
  size: Int,
  checksum: SHA3
) {
  def toByteArray: Barr = {
    val nameBytes = name.getBytes(UTF_8)
    ByteBuffer.allocate(nameBytes.length + 1 + 32 + 4)
      .put(checksum.hash)
      .put((fileType.ordinal + 1).toByte)
      .putInt(size)
      .put(nameBytes)
      .array()
  }
}

object Header {
  def apply(data: Barr): Header = {
    val buff = ByteBuffer.wrap(data)
    val hash = new Barr(32)
    buff.get(hash)
    val fileType = Header.Type(buff.get())
    val size = buff.getInt
    val name = buff.asCharBuffer().toString
    new Header(name, fileType, size, new SHA3(hash))
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
