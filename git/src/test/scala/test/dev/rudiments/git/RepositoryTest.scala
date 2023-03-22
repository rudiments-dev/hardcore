package test.dev.rudiments.git

import dev.rudiments.git.{Pack, Repository}
import dev.rudiments.git.Pack.PackObj
import dev.rudiments.utils.Log
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.file.{Files, Path}

@RunWith(classOf[JUnitRunner])
class RepositoryTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  private val repo = new Repository(dir)

  "can read packs" ignore {
    repo.read() //TODO fails of merge commit with pgp
  }
}
