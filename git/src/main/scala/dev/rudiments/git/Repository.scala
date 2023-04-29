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

  val errors: mutable.Map[SHA1, (Entry, String)] = mutable.HashMap.empty

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

  def readPack(hash: String): Unit = {
    val packEntries = Pack.readPack(root, hash).objects
    val initialObjects = objects.size
    val initialErros = errors.size
    packEntries.foreach {
      case (key, v@Entry(PackObj.Tree, size, data, _, _)) =>
        try {
          Tree(ZLib.unpack(data))
            .validate(size, key.string) match
            case Right(r) => objects.put(key, r)
            case Left(e) => errors.put(key, (v, e.getLocalizedMessage))
        } catch {
          case e: Exception => errors.put(key, (v, e.getLocalizedMessage))
        }
      case (key, v@Entry(PackObj.Commit, size, data, _, _)) =>
        try {
          Commit(ZLib.unpack(data))
            .validate(size, key.string) match
            case Right(r) => objects.put(key, r)
            case Left(e) => errors.put(key, (v, e.getLocalizedMessage))
        } catch {
          case e: Exception => errors.put(key, (v, e.getLocalizedMessage))
        }

      case (key, v@Entry(PackObj.Blob, size, data, _, _)) =>
        try {
          Blob(ZLib.unpack(data))
            .validate(size, key.string) match
            case Right(r) => objects.put(key, r)
            case Left(e) => errors.put(key, (v, e.getLocalizedMessage))
        } catch {
          case e: Exception => errors.put(key, (v, e.getLocalizedMessage))
        }

      case (key, v@Entry(PackObj.Tag, size, data, _, _)) =>
        try {
          Tag(ZLib.unpack(data))
            .validate(size, key.string) match
            case Right(r) => objects.put(key, r)
            case Left(e) => errors.put(key, (v, e.getLocalizedMessage))
        } catch {
          case e: Exception => errors.put(key, (v, e.getLocalizedMessage))
        }
      case (key, entry) => errors.put(key, (entry, "Parse are not implemented"))
    }

    if(errors.size - initialErros > 0) {
      log.error(s"Can't parse {${errors.size - initialErros}} entries into objects")
      val groupped: Map[PackObj, Iterable[(Entry, String)]] = errors.values.groupBy(_._1.what)
      val counted = groupped.map { (k, v) => k -> v.size }.toSeq.sortBy(_._2)
      log.error("Errors by groups: {}", counted.mkString(";"))
    }
    log.info(s"Parsed ${packEntries.size} entries into ${objects.size - initialObjects} objects")
  }
}
