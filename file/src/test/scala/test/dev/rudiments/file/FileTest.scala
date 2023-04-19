package test.dev.rudiments.file

import dev.rudiments.file.*
import dev.rudiments.utils.SHA3
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path


@RunWith(classOf[JUnitRunner])
class FileTest extends AnyWordSpec with Matchers {
  private val dir = Path.of("..", "file", "src", "test", "resources").toAbsolutePath

  private val f1 = new Header("1.txt", Header.Type.File, 10, SHA3("first file".getBytes(UTF_8)))
  private val f2 = new Header("2.txt", Header.Type.File,11, SHA3("second file".getBytes(UTF_8)))
  private val nested = new Header("nested", Header.Type.Dir, 11, SHA3(f2.toByteArray))

  "can read single file" in {
    Files.apply(dir.resolve(Path.of("example", "1.txt"))) should be(
      new One(f1, "first file".getBytes(UTF_8))
    )
  }

  "can read directory with file" in {
    Files.apply(dir.resolve(Path.of("example", "nested"))) should be (
      new Many(nested, Seq(f2))
    )
  }

  "can read nested directory" in {
    Files.apply(dir.resolve("example")) should be(
      new Many(
        new Header("example", Header.Type.Dir, 21, SHA3(f1.toByteArray ++ nested.toByteArray)),
        Seq(f1, nested)
      )
    )
  }
}
