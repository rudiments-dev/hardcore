package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.{Dir, FileAdapter, TextFile, UnknownFile}
import dev.rudiments.hardcore.http.ScalaRORouter
import io.circe.{Encoder, Json}

class ExampleFile extends LazyLogging {
  private implicit val space: Space = new Space()
  private implicit val fileEn: Encoder[Thing] = Encoder.instance {
    case dir: Dir => Json.obj(
      "type" -> Json.fromString("dir"),
      "path" -> Json.fromString(dir.absolutePath),
      "inside" -> Json.fromInt(dir.cache.state.size),
      "recursive" -> Json.fromLong(dir.totalRecursive)
    )
    case txt: TextFile => Json.obj(
      "type" -> Json.fromString("txt"),
      "path" -> Json.fromString(txt.absolutePath),
      "content" -> Json.arr(txt.cache.map(s => Json.fromString(s)):_*)
    )
    case unk: UnknownFile => Json.obj(
      "type" -> Json.fromString("unknown"),
      "path" -> Json.fromString(unk.absolutePath),
      "contentSize" -> Json.fromInt(unk.cache.size)
    )
  }
  private val exampleFile = Dir(".")
  val router = new ScalaRORouter[FileAdapter](
    Path(ID("file")),
    ScalaTypes.ScalaString,
    exampleFile
  )
}
