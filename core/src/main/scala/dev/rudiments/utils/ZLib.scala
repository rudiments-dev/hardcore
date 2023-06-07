package dev.rudiments.utils

import java.io.ByteArrayOutputStream
import java.util.zip.{Deflater, Inflater}

object ZLib {
  val DEFAULT_BUFFER_SIZE = 4096

  def pack(data: Array[Byte], size: Int = DEFAULT_BUFFER_SIZE): Array[Byte] = {
    val deflater = new Deflater()
    deflater.setLevel(Deflater.DEFAULT_COMPRESSION)
    deflater.setInput(data)

    val outputStream = new ByteArrayOutputStream(data.length)
    try {
      deflater.finish()
      val buffer = new Array[Byte](size)
      while (!deflater.finished) {
        val count = deflater.deflate(buffer)
        outputStream.write(buffer, 0, count)
      }
      outputStream.toByteArray
    } finally
      if (outputStream != null) outputStream.close()
  }

  def unpack(data: Array[Byte], size: Int = DEFAULT_BUFFER_SIZE): Array[Byte] = {
    val inflater = new Inflater()
    inflater.setInput(data)

    val outputStream = new ByteArrayOutputStream(data.length)
    try {
      val buffer = new Array[Byte](size)
      while (!inflater.finished) {
        val count = inflater.inflate(buffer)
        outputStream.write(buffer, 0, count)
      }
      outputStream.toByteArray
    } finally
      if (outputStream != null) outputStream.close()
  }
}
