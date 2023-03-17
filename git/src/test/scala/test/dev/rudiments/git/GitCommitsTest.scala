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

  "can read chain of commits" in {
    val result = for {
      first <- Reader.read(dir, "a3e9375e5ea70baf7d6a4ba343c59619aee1f2f0")
      tree <- Reader.read(dir, first.asInstanceOf[Commit].tree.toString)
      second <- Reader.read(dir, first.asInstanceOf[Commit].parent.get.toString)
    } yield (first, tree, second)

    result match {
      case Left(err) => throw err
      case Right(f, t, s) =>
        f.header should be("commit 238")
        t.header should be("tree 620")
        s.header should be("commit 275")
    }
  }
}
