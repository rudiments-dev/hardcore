package test.dev.rudiments.git

import dev.rudiments.git.Pack
import dev.rudiments.git.Pack.PackObj
import dev.rudiments.utils.Log
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.file.{Files, Path}

@RunWith(classOf[JUnitRunner])
class PackTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  "can read pack index" in {
    val hash = "8cc0a2aa174783656e7d32edb2993b578c957c2d"
    val readen = Pack.readPack(dir, hash)
    readen.objects.size should be (367)
  }
}
