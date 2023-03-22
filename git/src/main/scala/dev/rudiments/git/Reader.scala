package dev.rudiments.git

import dev.rudiments.utils.ZLib

import java.lang
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

object Reader {
  def read(repoDir: Path, hash: String): Either[Exception, GitObject] = {
    val subDir = hash.take(2)
    val fileName = hash.drop(2)
    val path = repoDir.resolve(Path.of(".git", "objects", subDir, fileName)).normalize()

    try {
      val compressed = Files.readAllBytes(path)
      val data = ZLib.unpack(compressed)
      if (!data.contains(0.toByte)) {
        Left(new IllegalArgumentException("Not found header-content delimiter"))
      } else {
        val headerIdx = data.indexOf(0.toByte)
        val header = new String(data.take(headerIdx), UTF_8)
        val content = data.drop(headerIdx + 1)
        header.split(" ").toList match
          case "blob" :: size :: Nil =>
            val blob = Blob(content)
            blob.validate(size.toInt, hash)
          case "tree" :: size :: Nil =>
            val tree = Tree(content)
            tree.validate(size.toInt, hash)
          case "commit" :: size :: Nil =>
            val commit = Commit(content)
            commit.validate(size.toInt, hash)
          case "object" :: _ => Left(new IllegalArgumentException("Tags not supported yet"))
          case other :: _ :: Nil => Left(new IllegalArgumentException(s"Not supported git object type: $other"))
          case _ => Left(new IllegalArgumentException("Wrong format of a git object"))
      }
    } catch {
      case e: Exception => Left(e)
    }
  }
}
