package test.dev.rudiments.git

import dev.rudiments.git.GitObject
import dev.rudiments.utils.SHA1
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GitBlobTest extends AnyWordSpec with Matchers {
  "Git BLOB" should {
    "fit header with example" in {
      // $echo -n "what is up, doc?" | git hash-object --stdin
      val blob = GitObject.Blob("what is up, doc?")
      blob.header should be ("blob 16")
      blob.hash.toString should be("bd9dbf5aae1a3862dd1526723246b20206e5fc37")
    }

    "fit with known hashes" in {
      val known = Map(
        "git compatible" -> "51e7ed8563dcc08a564795ead8899a8ced95838c",
        "sha-1" -> "ea9090c10ac8e06b8d50114e6816042d5a7e16d8"
      )

      val hashed = known.map((k, _) => k -> GitObject.Blob(k).hash.toString)
      hashed should be(known)
    }
  }
}
