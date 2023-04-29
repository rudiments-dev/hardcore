package dev.rudiments.git

import dev.rudiments.utils.{Hashed, SHA1, ZLib}

import java.lang
import java.lang.{IllegalStateException, StringBuffer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import scala.collection.mutable
import scala.util.matching.Regex

sealed trait GitObject(kind: String) {
  val header: String = s"$kind $size"
  def data: Array[Byte]
  def full: Array[Byte] = (header.getBytes(UTF_8) :+ 0.toByte) ++ data
  def hash: SHA1 = SHA1(full)
  def size: Int = data.length

  def validate(size: Int, hash: String): Either[Exception, this.type] =
    if (this.size != size) Left(new IllegalStateException(s"Invalid size, expected: ${this.size}"))
    else if (this.hash.toString != hash) Left(new IllegalStateException(s"Invalid hash, expected: ${this.hash}"))
    else Right(this)

  def objectPath: Path = {
    val subDir = hash.toString.take(2)
    val fileName = hash.toString.drop(2)
    Path.of(".git", "objects", subDir, fileName)
  }
}

final case class Blob(content: String) extends GitObject("blob") {
  override def data: Array[Byte] = content.getBytes(UTF_8)
}

object Blob {
  def apply(data: Array[Byte]): Blob = new Blob(new String(data, UTF_8))
}

final case class Tree(items: Seq[Tree.Item]) extends GitObject("tree") {
  override def data: Array[Byte] = items.foldLeft(Array.empty[Byte]) { (acc, el) => acc ++ el.toBytes }
}

object Tree {
  def apply(data: Array[Byte]): Tree = {
    if (!data.contains(0.toByte)) throw new IllegalArgumentException("Not found any delimiter")

    val items = mutable.Buffer.empty[Item]
    var start = 0
    while(start < data.length) {
      val div: Int = data.indexOf(0.toByte, start)
      val asString = new String(data.slice(start, div), UTF_8)
      asString.split(" ").toList match {
        case mode :: name :: Nil => items.addOne(Item(Mode(mode), name, new SHA1(data.slice(div + 1, div + 21))))
        case other => throw new IllegalArgumentException(s"Doesn't look like a tree item: `${other.mkString(";")}`")
      }
      start = div + 21
    }

    new Tree(items.toSeq)
  }

  enum Mode(val code: String):
    case File extends Mode("100644")
    case GroupFile extends Mode("100664") //TODO merge with File 100644?
    case Executable extends Mode("100755")
    case SymbolicLink extends Mode("120000")
    case SubTree extends Mode("40000")
    case SubModule extends Mode("160000")

  object Mode {
    def apply(code: String): Mode =
      values.find(_.code == code).getOrElse {
        throw new IllegalArgumentException(s"Not a mode code: $code")
      }
  }

  case class Item(mode: Mode, name: String, hash: SHA1) {
    def size: Int = name.length + 28 // `mode name\0sha-1` => 6 + 1 + name.size + 1 + 20
    def toBytes: Array[Byte] = ((mode.code + " " + name).getBytes(UTF_8) :+ 0.toByte) ++ hash.hash
  }
}

final case class Commit(
  tree: SHA1,
  parent: Option[SHA1],
  author: Commit.AuthRecord,
  committer: Commit.AuthRecord,
  message: String
) extends GitObject("commit") {
  override def data: Array[Byte] = {
    s"""tree $tree
       |parent ${parent.map(_.toString).getOrElse("NIL")}
       |author $author
       |committer $committer
       |
       |$message""".stripMargin
      .getBytes(UTF_8)
  }
}

object Commit {
  val signedPattern: Regex =
    """tree (\w{40})
      |parent (\w{40})
      |author (.+) <(.+)> (\d{10,20}) (.+)
      |committer (.+) <(.+)> (\d{10,20}) (.+)
      |gpgsig -----BEGIN PGP SIGNATURE-----
      |\s*
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{1,64}
      | -----END PGP SIGNATURE-----
      |\s*
      |\s*
      |(.*)""".stripMargin.r

  val signed2Pattern: Regex =
    """tree (\w{40})
      |parent (\w{40})
      |(parent (\w{40}))?
      |author (.+) <(.+)> (\d{10,20}) (.+)
      |committer (.+) <(.+)> (\d{10,20}) (.+)
      |gpgsig -----BEGIN PGP SIGNATURE-----
      |\s*
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{64}
      | .{1,64}
      | -----END PGP SIGNATURE-----
      |\s*
      |\s*
      |(.*)""".stripMargin.r

  val commitPattern: Regex =
    """tree (\w{40})
      |parent (\w{40})
      |author (.+) <(.+)> (\d{10,20}) (.+)
      |committer (.+) <(.+)> (\d{10,20}) (.+)
      |
      |(.*)""".stripMargin.r

  val mergePattern: Regex =
    """tree (\w{40})
      |parent (\w{40})
      |(parent (\w{40}))?
      |author (.+) <(.+)> (\d{10,20}) (.+)
      |committer (.+) <(.+)> (\d{10,20}) (.+)
      |
      |(.*)""".stripMargin.r

  def apply(data: Array[Byte]): Commit = {
    val asString = new String(data, UTF_8)
    try {
      commitPattern.findFirstMatchIn(asString).map { m =>
        new Commit(
          SHA1.fromHex(m.group(1)),
          Some(SHA1.fromHex(m.group(2))),
          AuthRecord(m.group(3), m.group(4), Instant.ofEpochMilli(m.group(5).toLong).atZone(ZoneId.of(m.group(6)))),
          AuthRecord(m.group(7), m.group(8), Instant.ofEpochMilli(m.group(9).toLong).atZone(ZoneId.of(m.group(10)))),
          m.group(11)
        )
      }.getOrElse {
        mergePattern.findFirstMatchIn(asString).map { m =>
          new Commit(
            SHA1.fromHex(m.group(1)),
            Some(SHA1.fromHex(m.group(2))),
            AuthRecord(m.group(5), m.group(6), Instant.ofEpochMilli(m.group(7).toLong).atZone(ZoneId.of(m.group(8)))),
            AuthRecord(m.group(9), m.group(10), Instant.ofEpochMilli(m.group(11).toLong).atZone(ZoneId.of(m.group(12)))),
            m.group(13)
          )
        }.getOrElse {
          signedPattern.findFirstMatchIn(asString).map { m =>
            new Commit(
              SHA1.fromHex(m.group(1)),
              Some(SHA1.fromHex(m.group(2))),
              AuthRecord(m.group(3), m.group(4), Instant.ofEpochMilli(m.group(5).toLong).atZone(ZoneId.of(m.group(6)))),
              AuthRecord(m.group(7), m.group(8), Instant.ofEpochMilli(m.group(9).toLong).atZone(ZoneId.of(m.group(10)))),
              m.group(11)
            )
          }.getOrElse {
            signed2Pattern.findFirstMatchIn(asString).map { m =>
              new Commit(
                SHA1.fromHex(m.group(1)),
                Some(SHA1.fromHex(m.group(2))),
                AuthRecord(m.group(5), m.group(6), Instant.ofEpochMilli(m.group(7).toLong).atZone(ZoneId.of(m.group(8)))),
                AuthRecord(m.group(9), m.group(10), Instant.ofEpochMilli(m.group(11).toLong).atZone(ZoneId.of(m.group(12)))),
                m.group(13)
              )
            }.getOrElse {
              throw new IllegalArgumentException(s"Can't read commit: ${asString.take(50)}")
            }
          }
        }
      }
    } catch {
      case e: Exception => throw e
    }
  }

  private val tsFormat: DateTimeFormatter = new DateTimeFormatterBuilder()
    .appendValue(ChronoField.INSTANT_SECONDS, 1, 19, SignStyle.NEVER)
    .appendValue(ChronoField.MILLI_OF_SECOND, 3)
    .appendLiteral(" ")
    .appendOffset("+HHMMss", "0")
    .toFormatter();

  case class AuthRecord(name: String, email: String, when: ZonedDateTime) {
    override def toString: String = s"$name $email ${tsFormat.format(when)}"
  }

  object AuthRecord {
    def parse(s: String): AuthRecord = {
      s.split(" ").toList match
        case name :: mail :: time :: zone :: Nil =>
          val z = ZoneId.of(zone)
          val when = Instant.ofEpochMilli(time.toLong).atZone(z)
          new AuthRecord(name, mail, when)
        case name1 :: name2 :: mail :: time :: zone :: Nil =>
          val z = ZoneId.of(zone)
          val when = Instant.ofEpochMilli(time.toLong).atZone(z)
          new AuthRecord(name1 + " " + name2, mail, when)
    }
  }
}