package dev.rudiments.git

import dev.rudiments.utils.{Hashed, SHA1}

case class GitBlob(s: String, h: String, hash: SHA1)

object GitBlob {
  def apply(s: String): GitBlob = {
    val h = "blob " + s.getBytes(Hashed.utf8).length + "\u0000"
    new GitBlob(s, h, SHA1(h + s))
  }
}
