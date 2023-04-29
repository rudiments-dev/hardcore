package dev.rudiments.git

import dev.rudiments.git.Commit.Field.{Author, Parent}
import dev.rudiments.utils.{Hashed, SHA1, ZLib}

import java.lang
import java.lang.{IllegalStateException, StringBuffer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import scala.collection.immutable.ArraySeq
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

final case class Blob(content: Seq[Byte]) extends GitObject("blob") {
  override def data: Array[Byte] = content.toArray[Byte]
  lazy val asString: String = new String(data, UTF_8)
}
object Blob {
  def apply(data: Array[Byte]): Blob = new Blob(data.toSeq)
  def apply(str: String): Blob = new Blob(str.getBytes(UTF_8).toSeq)
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
        case mode :: name => items.addOne(Item(Mode(mode), name.mkString(" "), new SHA1(data.slice(div + 1, div + 21))))
        case other => throw new IllegalArgumentException(s"Doesn't look like a tree item: `${other.mkString(";")}`")
      }
      start = div + 21
    }

    new Tree(items.toSeq)
  }

  enum Mode(val code: String):
    case File extends Mode("100644")
    case GroupFile extends Mode("100664")
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
  parent: Seq[SHA1],
  author: Commit.AuthRecord,
  committer: Commit.AuthRecord,
  message: String,
  signature: Option[String] = None,
  originalData: Option[Seq[Byte]] = None
) extends GitObject("commit") {
  override def data: Array[Byte] = {
    originalData match
      case None =>
        val buff = new StringBuilder()
        buff.append(s"tree $tree\n")
        parent.foreach { p => buff.append(s"parent $p\n") }
        buff.append(s"author $author\n")
        buff.append(s"committer $committer\n")
        signature.foreach { s => buff
          .append("gpgsig -----BEGIN PGP SIGNATURE-----\n")
          .append(s)
          .append(" -----END PGP SIGNATURE-----")
        }
        buff.append(s"\n\n$message")
        buff.toString().getBytes(UTF_8)

      case Some(msg) => msg.toArray[Byte]
  }
}
object Commit {
  enum Field(val regex: Regex):
    case Tree extends Field(raw"tree (\w{40})".r)
    case Parent extends Field(raw"\nparent (\w{40})".r)
    case Author extends Field(raw"\nauthor (.+) <(.+)> (\d{10,20}) (.+)".r)
    case Committer extends Field(raw"\ncommitter (.+) <(.+)> (\d{10,20}) (.+)".r)
    case Signature extends Field(raw"\ngpgsig -----BEGIN PGP SIGNATURE-----(.*\n)* -----END PGP SIGNATURE-----".r)
    case Message extends Field(raw"(-----END PGP SIGNATURE-----)?\n\n(.*\n?)*".r)

  def apply(data: Array[Byte]): Commit = {
    val str = new String(data, UTF_8)
    val asMap = Field.values.toSeq.map { f => f -> f.regex.findAllMatchIn(str) }.toMap
    val signature = asMap(Field.Signature)
    val candidate = new Commit(
      SHA1.fromHex(asMap(Field.Tree).map(_.group(1)).toSeq.head),
      asMap(Field.Parent).map(_.group(1)).toSeq.map(SHA1.fromHex),
      asMap(Field.Author).map(AuthRecord.apply).toSeq.head,
      asMap(Field.Committer).map(AuthRecord.apply).toSeq.head,
      asMap(Field.Message).mkString("\n"),
      if(signature.nonEmpty) Some(signature.mkString("\n")) else None
    )

    if(new String(candidate.data, UTF_8) == str) {
      candidate
    } else {
      candidate.copy(originalData = Some(data.toSeq))
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
    def apply(reg: Regex.Match): AuthRecord = new AuthRecord(
      reg.group(1),
      reg.group(2),
      Instant.ofEpochMilli(reg.group(3).toLong)
        .atZone(ZoneId.of(reg.group(4)))
    )
  }
}

final case class Tag(
  link: SHA1,
  tagType: String,
  tag: String,
  tagger: Option[Commit.AuthRecord],
  message: String,
  signature: Option[String] = None,
  originalData: Option[Seq[Byte]] = None
) extends GitObject("tag") {
  override def data: Array[Byte] = {
    originalData match
      case None =>
        val buff = new StringBuilder()
        buff.append(s"object $link\n")
        buff.append(s"type $tagType\n")
        buff.append(s"tag $tag\n")
        tagger.foreach { t => buff.append(s"tagger $t\n") }
        signature.foreach { s =>
          buff
            .append("gpgsig -----BEGIN PGP SIGNATURE-----\n")
            .append(s)
            .append(" -----END PGP SIGNATURE-----")
        }
        buff.append(s"\n\n$message")
        buff.toString().getBytes(UTF_8)

      case Some(msg) => msg.toArray[Byte]
  }
}
object Tag {
  enum Field(val regex: Regex):
    case Object extends Field(raw"object (\w{40})".r)
    case TagType extends Field(raw"\ntype (\w+)".r)
    case TagName extends Field(raw"\ntag (\w+)".r)
    case Tagger extends Field(raw"\ntagger (.+) <(.+)> (\d{10,20}) (.+)".r)
    case Signature extends Field(raw"\ngpgsig -----BEGIN PGP SIGNATURE-----(.*\n)* -----END PGP SIGNATURE-----".r)
    case Message extends Field(raw"(-----END PGP SIGNATURE-----)?\n\n(.*\n?)*".r)

  def apply(data: Array[Byte]): Tag = {
    val str = new String(data, UTF_8)
    val asMap = Field.values.toSeq.map { f => f -> f.regex.findAllMatchIn(str) }.toMap
    val signature = asMap(Field.Signature)
    val candidate = new Tag(
      SHA1.fromHex(asMap(Field.Object).map(_.group(1)).toSeq.head),
      asMap(Field.TagType).map(_.group(1)).toSeq.head,
      asMap(Field.TagName).map(_.group(1)).toSeq.head,
      asMap(Field.Tagger).map(Commit.AuthRecord.apply).toSeq.headOption,
      asMap(Field.Message).mkString("\n"),
      if(signature.nonEmpty) Some(signature.mkString("\n")) else None
    )

    if (new String(candidate.data, UTF_8) == str) {
      candidate
    } else {
      candidate.copy(originalData = Some(data.toSeq))
    }
  }
}
