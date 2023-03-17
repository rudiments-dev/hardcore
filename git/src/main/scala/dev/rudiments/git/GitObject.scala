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
    case Executable extends Mode("100755")
    case SymbolicLink extends Mode("120000")
    case SubTree extends Mode("40000")

  object Mode {
    def apply(code: String): Mode =
      values.find(_.code == code).getOrElse(throw new IllegalArgumentException(s"Not a mode code: $code"))
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
  def apply(data: Array[Byte]): Commit = {
    new String(data, UTF_8).split("\n\n").toList match {
      case header :: message :: Nil =>
        header.split("\n").toList match {
          case t :: p :: a :: c :: Nil
            if t.startsWith("tree ") &&
              p.startsWith("parent ") &&
              a.startsWith("author ") &&
              c.startsWith("committer ") =>
            new Commit(
              SHA1.fromHex(t.drop(5)),
              if (p.drop(7) == "NIL") None else Some(SHA1.fromHex(p.drop(7))),
              AuthRecord.parse(a.drop(7)),
              AuthRecord.parse(c.drop(10)),
              message
            )
          case _ => throw new IllegalArgumentException("Can't read commit header")
        }
      case _ => throw new IllegalArgumentException("Can't read commit")
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
    }
  }
}