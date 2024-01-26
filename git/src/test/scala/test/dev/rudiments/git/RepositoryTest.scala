package test.dev.rudiments.git

import dev.rudiments.git.{Pack, Repository}
import dev.rudiments.git.Pack.PackObj
import dev.rudiments.utils.Log
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class RepositoryTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  private val repo = new Repository(dir)

  "can read packs" in {
    repo.read() //TODO fails on initial commit
  }
}
