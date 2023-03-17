package dev.rudiments.utils

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.{Base64, HexFormat}

trait Hashed(hash: Array[Byte]) {
  lazy val bigInteget: BigInteger = new BigInteger(1, hash)
  lazy val string: String = String.format("%064x", bigInteget)

  override def toString: String = string

  override def hashCode(): Int = this.hash.toList.hashCode()
}

object Hashed {
  val hexFormat: HexFormat = HexFormat.of()
}

case class SHA1(hash: Array[Byte]) extends Hashed(hash) {
  override lazy val string: String = String.format("%040x", bigInteget)

  override def equals(obj: Any): Boolean = obj match {
    case other: SHA1 => this.hash.sameElements(other.hash)
    case _ => false
  }
}

object SHA1 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA-1")

  def apply(s: String): SHA1 = this.apply(s.getBytes(UTF_8))

  def apply(b: Array[Byte]): SHA1 = new SHA1(digester.digest(b))

  def fromHex(hex: String): SHA1 = new SHA1(Hashed.hexFormat.parseHex(hex))
}

case class SHA256(hash: Array[Byte]) extends Hashed(hash) {
  override def equals(obj: Any): Boolean = obj match {
    case other: SHA256 => this.hash.sameElements(other.hash)
    case _ => false
  }
}

object SHA256 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA-256")

  def apply(s: String): SHA256 = this.apply(s.getBytes(UTF_8))
  def apply(b: Array[Byte]): SHA256 = new SHA256(digester.digest(b))
}


case class SHA3(hash: Array[Byte]) extends Hashed(hash) {
  override def equals(obj: Any): Boolean = obj match {
    case other: SHA3 => this.hash.sameElements(other.hash)
    case _ => false
  }
}

object SHA3 {
  val digester: MessageDigest = MessageDigest.getInstance("SHA3-256")

  def apply(s: String): SHA3 = this.apply(s.getBytes(UTF_8))

  def apply(b: Array[Byte]): SHA3 = new SHA3(digester.digest(b))
}
