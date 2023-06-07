package dev.rudiments.git

import dev.rudiments.utils.{Hashed, ZLib}

import java.nio.file.{Files, Path}

object Writer {
  def write(repoDir: Path, obj: GitObject): Status = {
    val path = repoDir.resolve(obj.objectPath).normalize()
    import java.nio.file.StandardOpenOption._
    try {
      val compressed = ZLib.pack(obj.full)
      Files.write(path, compressed, CREATE_NEW, WRITE)
      Status.Success
    } catch {
      case e: Exception => Status.Failure(e)
    }
  }

  def deleteIfExist(repoDir: Path, obj: GitObject): Status = {
    val path = repoDir.resolve(obj.objectPath).normalize()
    import java.nio.file.StandardOpenOption._
    try {
      Files.deleteIfExists(path)
      Status.Success
    } catch {
      case e: Exception => Status.Failure(e)
    }
  }

  enum Status:
    case Success
    case Failure(e: Exception)
}
