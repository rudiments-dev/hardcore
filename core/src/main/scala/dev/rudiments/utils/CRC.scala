package dev.rudiments.utils

case class CRC(crc: Array[Byte])

object CRC {
  def apply(data: Array[Byte]): CRC = ??? // how to verify?
}
