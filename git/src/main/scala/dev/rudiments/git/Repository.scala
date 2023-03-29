package dev.rudiments.git

import java.nio.charset.StandardCharsets.UTF_8
import dev.rudiments.git.Pack.{Entry, PackObj}
import dev.rudiments.utils.{Log, SHA1, ZLib}

import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.collection.mutable

class Repository(root: Path) extends Log {
  var head: Commit = _

  val branches: mutable.Buffer[String] = mutable.Buffer.empty
  val heads: mutable.Buffer[String] = mutable.Buffer.empty
  val tags: mutable.Buffer[String] = mutable.Buffer.empty
  val objects: mutable.Map[SHA1, GitObject] =  mutable.HashMap.empty

  private val packPath = root.resolve(Path.of(".git", "objects", "pack"))

  def read(): Unit = {
    val packPattern = "pack-(\\w+).pack".r

    Files.list(packPath).filter { f =>
      val s = f.getFileName.toString
      s.startsWith("pack") && s.endsWith(".pack")
    }.forEach { p =>
      packPattern.findFirstMatchIn(p.getFileName.toString).map(_.group(1)).foreach { pack =>
        log.info("Reading pack {}", pack)
        readPack(pack)
      }
    }
  }

  def readPack(hash: String): Unit = Pack.readPack(root, hash).objects.foreach {
    case (key, Entry(PackObj.Tree, _, data, _, _)) =>
      objects.put(key, Tree(ZLib.unpack(data)))
    case (key, Entry(PackObj.Commit, _, data, _, _)) =>
      try {
        objects.put(key, Commit(ZLib.unpack(data)))
      } catch {
        case e: Exception => log.error("Failed commit {}", key)
      }

    case (key, Entry(PackObj.Blob, _, data, _, _)) =>
      objects.put(key, Blob(ZLib.unpack(data)))

    case (key, Entry(PackObj.Tag, _, _, _, _)) => // TODO
    case (key, entry) => // filtered
  }
}
