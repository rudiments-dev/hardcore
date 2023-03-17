package test.dev.rudiments.utils

import dev.rudiments.utils.{SHA256, SHA3, SHA1}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HashedTest extends AnyWordSpec with Matchers {
  "SHA-1 hash" should {
    "fit with known hashes" in {
      val known = Map(
        "" -> "da39a3ee5e6b4b0d3255bfef95601890afd80709",
        "The quick brown fox jumps over the lazy dog" -> "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"
      )

      val hashed = known.map((k, _) => k -> SHA1(k).toString)
      hashed should be(known)
    }
  }

  "SHA-256 hash" should {
    "fit with known hashes" in {
      val known = Map(
        "" -> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "sha-256" -> "3128f8ac2988e171a53782b144b98a5c2ee723489c8b220cece002916fbc71e2",
        "The quick brown fox jumps over the lazy dog" -> "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
      )

      val hashed = known.map((k, _) => k -> SHA256(k).toString)
      hashed should be (known)
    }
  }

  "SHA3-256 hash" should {
    "fit with known hashes" in {
      val known = Map(
        "" -> "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
        "sha3-256" -> "5d90e98d57bb0f24f935080cb1bab85eaedec5d958fa979cd53e8147e32111e1",
        "The quick brown fox jumps over the lazy dog" -> "69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04"
      )

      val hashed = known.map((k, _) => k -> SHA3(k).toString)
      hashed should be(known)
    }
  }
}
