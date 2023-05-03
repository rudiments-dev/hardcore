package test.dev.rudiments.git

import dev.rudiments.git.{Deltified, Pack, RefDelta}
import dev.rudiments.git.Pack.PackObj
import dev.rudiments.utils.{Hashed, Log}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

@RunWith(classOf[JUnitRunner])
class PackTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  "can read pack index" in {
    val hash = "8cc0a2aa174783656e7d32edb2993b578c957c2d"
    val readen = Pack.readPack(dir, hash)
    readen.objects.size should be (367)
  }

  "can parse ref delta from byte array" in {
    val hex = "e5614b53 b3699d9fc08d41135b16a4a875b0fc68 789c 6b38cad8718071034702 00 142c03a8".replace(" ", "")
    val data = Hashed.hexFormat.parseHex(hex)
    val delta = RefDelta(data)

    delta.original should be (Hashed.hexFormat.parseHex("80c50188c001b00860").toSeq)

    val buff = ByteBuffer.wrap(delta.original.toArray[Byte])
    buff.position() should be (0)
    buff.get() should be (0x80.toByte)
    Deltified.variableSize(buff) should be (197) // offset?
    buff.position() should be (3)
    Deltified.variableSize(buff) should be (24584) // result size!
    buff.position() should be (6)
    Deltified.variableSize(buff) should be (1072)
    buff.position() should be (8)
    buff.get() should be (0x60.toByte)
  }

  // e5614b53 b3699d9fc08d41135b16a4a875b0fc68 789c 6b38cad8718071034702 00 142c03a8
  // e5614b53 b3699d9fc08d41135b16a4a875b0fc68 -> f64062be 71facf76fba819b036562a513e6ba1b1
  // 789c 6b38cad8718071034702 00 142c03a8
  //      80 c501 88c001 b008 60
  // -128 -59 1 -120 -64 1 -80 8 96
/*
  10000000
  11000101
  00000001
  10001000
  11000000
  00000001
  10110000
  00001000
  01100000
*/
}
