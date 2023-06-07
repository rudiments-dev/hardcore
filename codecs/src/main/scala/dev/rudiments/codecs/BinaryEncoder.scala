package dev.rudiments.codecs

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

trait BinaryEncoder[A] {
  extension (a: A) def encode(): Array[Byte]
}

object BinaryEncoder {
  given intEncoder: BinaryEncoder[Int] with
    extension (a: Int)
      def encode(): Array[Byte] = ByteBuffer.allocate(4).putInt(a).array()

  given longEncoder: BinaryEncoder[Long] with
    extension (a: Long)
      def encode(): Array[Byte] = ByteBuffer.allocate(8).putLong(a).array()

  given stringEncoder: BinaryEncoder[String] with
    extension (a: String)
      def encode(): Array[Byte] = {
        val arr = a.getBytes(UTF_8)
        ByteBuffer.allocate(arr.size + 4)
          .putInt(arr.size)
          .put(arr)
          .array()
      }

  given optionEncoder[T: BinaryEncoder]: BinaryEncoder[Option[T]] with
    extension (a: Option[T])
      def encode(): Array[Byte] = a match
        case Some(v) => v.encode()
        case None => Array.empty[Byte]

  given product2Encoder[A: BinaryEncoder, B: BinaryEncoder]: BinaryEncoder[Product2[A, B]] with
    extension (p: Product2[A, B])
      def encode(): Array[Byte] = {
        val a = p._1.encode()
        val b = p._2.encode()
        a ++ b
      }

  //TODO study io.circe.EncoderDerivation & io.circe.Derivation#summonEncodersRec
  given tuple2Encoder[A: BinaryEncoder, B: BinaryEncoder]: BinaryEncoder[(A, B)] with
    extension (t: (A, B))
      def encode(): Array[Byte] = t._1.encode() ++ t._2.encode()

  given seqEncoder[T: BinaryEncoder]: BinaryEncoder[Seq[T]] with
    extension (a: Seq[T])
      def encode(): Array[Byte] = {
        val arr = a.foldLeft(Seq.empty[Byte]) { (acc, el) => acc ++ el.encode() }
        ByteBuffer.allocate(arr.size + 4)
          .putInt(arr.size)
          .put(arr.toArray[Byte])
          .array()
      }
}
