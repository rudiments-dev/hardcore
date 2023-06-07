package dev.rudiments.codecs

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

object BytesCodec {
  def encodeStrings(strings: Seq[String]): Array[Byte] = {
    val bytes = strings.map { s =>
      s.getBytes(UTF_8)
    }
    val size = 4 + bytes.size * 4 + bytes.map(_.length).sum // arr size + each element size + data
    val buff = ByteBuffer.allocate(size).putInt(bytes.size)
    bytes.foreach(b => buff.putInt(b.length).put(b))
    buff.array()
  }

  def decodeString(bytes: Array[Byte]): Seq[String] = {
    val buff = ByteBuffer.wrap(bytes)
    (0 to buff.getInt).map { _ =>
      val size = buff.getInt
      val arr = new Array[Byte](size)
      buff.get(arr)
      new String(arr, UTF_8)
    }
  }
}
