package test.dev.rudiments.git

import dev.rudiments.git.{Commit, Reader}
import dev.rudiments.utils.Log
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.file.{Files, Path}

@RunWith(classOf[JUnitRunner])
class GitCommitsTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  "can read chain of commits till the first one" ignore {
    var h = "a3e9375e5ea70baf7d6a4ba343c59619aee1f2f0"
    var i = 0; // up to 37

    while(h != "SUCCESS" || h != "FAIL") {
      Reader.read(dir, h) match {
        case Right(c: Commit) if c.parent.nonEmpty =>
          h = c.parent.head.toString
          i += 1
        case Right(_) => h = "SUCCESS"
        case Left(err) => h = "FAIL"
          throw err
      }
    }

    //5d631a5fb3318f3cf14ba3c7e0aba9e6674b8944
  }
}
