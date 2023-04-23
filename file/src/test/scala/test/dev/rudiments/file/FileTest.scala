package test.dev.rudiments.file

import dev.rudiments.file.{Repository, *}
import dev.rudiments.utils.SHA3
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path


@RunWith(classOf[JUnitRunner])
class FileTest extends AnyWordSpec with Matchers {
  private val dir = Path.of("..", "file", "src", "test", "resources", "example").toAbsolutePath
  val repo = new Repository(dir)

  "can read repository" in {
    repo.read()
    repo.state.size should be (4)
    repo.state.toMap should be (
      Map[Seq[String], FileData](
        Seq.empty -> Dir(Seq("nested"), Seq("1.txt")),
        Seq("nested") -> Dir(Seq.empty, Seq("2.txt")),
        Seq("nested", "2.txt") -> Blob("second file".getBytes(UTF_8)),
        Seq("1.txt") -> Blob("first file".getBytes(UTF_8))
      )
    )
  }
}
