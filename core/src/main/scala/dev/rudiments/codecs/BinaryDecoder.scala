package dev.rudiments.codecs

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

trait BinaryDecoder[A] {
  extension (arr: Array[Byte]) def decode(): A
}

object BinaryDecoder {
  given intDecoder: BinaryDecoder[Int] with
    extension (arr: Array[Byte])
      def decode(): Int = ByteBuffer.wrap(arr).getInt

  given longDecoder: BinaryDecoder[Long] with
    extension (arr: Array[Byte])
      def decode(): Long = ByteBuffer.wrap(arr).getLong()

  given stringDecoder: BinaryDecoder[String] with
    extension (arr: Array[Byte])
      def decode(): String = {
        val buff = ByteBuffer.wrap(arr)
        val size = buff.getInt
        val a: Array[Byte] = new Array(size)
        buff.get(a)
        new String(a, UTF_8)
      }

  given optionDecoder[T: BinaryDecoder]: BinaryDecoder[Option[T]] with
    extension (arr: Array[Byte])
      def decode(): Option[T] = {
        if (arr.isEmpty) None
        else arr.decode()
      }

  given iterableDecoder[T: BinaryDecoder]: BinaryDecoder[Iterable[T]] with
    extension (arr: Array[Byte])
      def decode(): Iterable[T] = {
        val buff = ByteBuffer.wrap(arr)
        val size = buff.getInt
        val a: Array[Byte] = new Array(size)
        ???

      }
}