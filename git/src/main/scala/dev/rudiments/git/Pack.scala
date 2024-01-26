package dev.rudiments.git

import dev.rudiments.utils.{CRC, SHA1, ZLib}

import java.io.{FileInputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.nio.file.StandardOpenOption.READ
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success}

case class Pack(objects: List[(SHA1, Pack.Entry)]) {
  lazy val hashIndex: Map[SHA1, Pack.Entry] = objects.toMap
  //need <index, hash, entry> maps?
}

object Pack {
  enum PackObj:
    case Commit, Tree, Blob, Tag, ForFutureUse, OffsetDelta, RefDelta

  object PackObj {
    private val objMask = (7 << 4).toByte
    def apply(b: Byte): PackObj = {
      val code = (b & objMask) >> 4
      PackObj.values(code - 1)
    }
  }

  case class Entry(what: PackObj, size: Int, data: Array[Byte], from: Int, to: Int)

  def readIdx(repo: Path, hash: String, size: Int): List[(SHA1, Int)] = {
    val path = repo.resolve(Path.of(".git", "objects", "pack", s"pack-$hash.idx"))
    val data = Files.readAllBytes(path)
    val header = Array(255, 116, 79, 99, 0, 0, 0, 2).map(_.toByte)
    assume(data.slice(0, 8).sameElements(header), "Expecting magic bytes and version 2")
    //TODO use fanout when search in file?
    val shaFrom = 8 + 256 * 4
    val shaUntil = shaFrom + size * 20
    val crcUntil = shaUntil + size * 4
    val offsetsUntil = crcUntil + size * 4
    val trailerUntil = offsetsUntil + 40
    val sha = data.slice(shaFrom, shaUntil).grouped(20).map(h => new SHA1(ArraySeq.unsafeWrapArray(h))).toList
    val crc = data.slice(shaUntil, crcUntil).grouped(4).map(c => new CRC(c)).toList
    val offsets = data.slice(crcUntil, offsetsUntil).grouped(4).map(b => ByteBuffer.wrap(b).getInt).toList
    val trailer = data.slice(offsetsUntil, offsetsUntil + 40) //TODO verify integrity

    assume(data.length == trailerUntil)
    assume(sha.size == crc.size, "List of hashes and list of CRC should be the same")
    assume(sha.size == offsets.size, "List of hashes and list of offsets should be the same")
    sha.zip(offsets).sortBy(_._2)
  }

  def readPack(repo: Path, hash: String): Pack = {
    val path = repo.resolve(Path.of(".git", "objects", "pack", s"pack-$hash.pack"))

    val bytes = Files.readAllBytes(path)
    val count = readPackMeta(bytes)
    val idx = readIdx(repo, hash, count)

    if(idx.nonEmpty) {
      val tail = new SHA1(Seq.empty) -> (bytes.length - 20)

      val pack = (idx :+ tail).sliding(2).map {
        case f :: s :: Nil => f._1 -> readEntry(bytes, f._2, s._2)
        case _ => ???
      }.toList

      Pack(pack)
    } else {
      Pack(Nil)
    }
  }

  private def readPackMeta(bytes: Array[Byte]): Int = {
    val buf = ByteBuffer.allocate(4)

    val packHeader = new String(bytes.take(4), UTF_8)
    assume(packHeader == "PACK", "Not a pack")

    val version = buf.put(0, bytes.slice(4, 8)).getInt
    assume(version == 2, "Only version 2 is supported")

    buf.clear()
    buf.put(0, bytes.slice(8, 12)).getInt
  }

  private def readEntry(bytes: Array[Byte], from: Int, until: Int): Entry = { //TODO move to ByteUtils & rewrite with ByteBuffer
    var address = from
    var b = bytes(address)
    val objType = PackObj(b)

    var size = b & 0xF
    var cursor = 4

    while (nextIsSize(b) && cursor <= 32) {
      address += 1
      b = bytes(address)
      val delta = (b & 0x7F) << cursor
      size = size | delta
      cursor += 7
    }

    val data = bytes.slice(address + 1, until)
    //val data = ZLib.unpack(compressed)
    Entry(objType, size, data, address + 1, until)
  }

  private def nextIsSize(b: Byte): Boolean = (b & 0x80).toByte == -128.toByte
}
