package dev.rudiments.git

import dev.rudiments.git.Pack.PackObj

import java.nio.ByteBuffer

implicit class ByteBufferOps(buff: ByteBuffer) {
  def unsigned(): Int = buff.get() & 0xFF
  def getUBytes(n: Int): Seq[Byte] = {
    val arr = new Array[Byte](n)
    buff.get(arr)
    arr.toSeq
  }
}

implicit class ByteUtils(b: Byte) {
  def mask(c: Byte): Boolean = (b & c) != 0
  def bitIsSet(bit: Int): Boolean = {
    if(bit > 7 || bit < 0) throw new IllegalArgumentException(s"Not supported bit position $bit")
    else this.mask((1 << bit).toByte)
  }
}

implicit class IntForByteUtils(i: Int) {
  def mask(c: Int): Boolean = (i & c) != 0
  def bitIsSet(bit: Int): Boolean = {
    if (bit > 7 || bit < 0) throw new IllegalArgumentException(s"Not supported bit position $bit")
    else this.mask(1 << bit)
  }
}

object ByteUtils {
  def variableSize(buff: ByteBuffer): Int = {
    var cursor = 0
    var size: Int = 0

    while {
      val b = buff.get()
      size = size | ((b & 0x7F) << cursor)
      cursor += 7

      nextIsSize(b) && cursor <= 32
    } do ()

    size
  }

  //TODO variable size with PackObject type

  def delta(buff: ByteBuffer): Deltified = {
    val head = buff.unsigned()
    if(head.bitIsSet(7)) {
      var offset = 0
      if(head.bitIsSet(0)) offset = buff.unsigned()
      if(head.bitIsSet(1)) offset |= (buff.unsigned() << 8)
      if(head.bitIsSet(2)) offset |= (buff.unsigned() << 16)
      if(head.bitIsSet(3)) offset |= (buff.unsigned() << 24)

      var size = 0
      if(head.bitIsSet(4)) size = buff.unsigned()
      if(head.bitIsSet(5)) size |= (buff.unsigned() << 8)
      if(head.bitIsSet(6)) size |= (buff.unsigned() << 16)

      if(size == 0) size = 0x10000

      Deltified.Copy(offset, size)
    } else {
      if(head != 0) {
        Deltified.Add(head, buff.getUBytes(head))
      } else {
        throw new IllegalArgumentException("Not supported 0000 0000 byte")
      }
    }
  }

  def nextIsSize(b: Byte): Boolean = (b & 0x80).toByte == -128.toByte
}
