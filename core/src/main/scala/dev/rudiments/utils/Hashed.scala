package dev.rudiments.utils

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.{Base64, HexFormat}
import scala.collection.immutable.ArraySeq

sealed trait Hashed(hash: Seq[Byte]) {
  lazy val asArray: Array[Byte] = hash.toArray[Byte]
  lazy val bigInteger: BigInteger = new BigInteger(1, asArray)
  lazy val string: String = String.format("%064x", bigInteger)

  override def toString: String = string

  override def hashCode(): Int = this.hash.toList.hashCode()
}

object Hashed {
  val hexFormat: HexFormat = HexFormat.of()
}

final case class SHA1(hash: Seq[Byte]) extends Hashed(hash) {
  override lazy val string: String = String.format("%040x", bigInteger)
}

object SHA1 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA-1")

  def apply(s: String): SHA1 = this.apply(s.getBytes(UTF_8))
  def apply(b: Array[Byte]): SHA1 = new SHA1(ArraySeq.unsafeWrapArray(digester.digest(b)))

  def fromHex(hex: String): SHA1 = new SHA1(ArraySeq.unsafeWrapArray(Hashed.hexFormat.parseHex(hex)))
}

final case class SHA256(hash: Seq[Byte]) extends Hashed(hash)

object SHA256 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA-256")

  def apply(s: String): SHA256 = this.apply(s.getBytes(UTF_8))
  def apply(b: Array[Byte]): SHA256 = new SHA256(ArraySeq.unsafeWrapArray(digester.digest(b)))

  def fromHex(hex: String): SHA256 = new SHA256(ArraySeq.unsafeWrapArray(Hashed.hexFormat.parseHex(hex)))
}


final case class SHA3(hash: Seq[Byte]) extends Hashed(hash)

object SHA3 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA3-256")

  def apply(s: String): SHA3 = this.apply(s.getBytes(UTF_8))
  def apply(b: Array[Byte]): SHA3 = new SHA3(ArraySeq.unsafeWrapArray(digester.digest(b)))

  def fromHex(hex: String): SHA3 = new SHA3(ArraySeq.unsafeWrapArray(Hashed.hexFormat.parseHex(hex)))
  
  def empty: SHA3 = new SHA3(ArraySeq.unsafeWrapArray(digester.digest(Array.empty[Byte])))
}
