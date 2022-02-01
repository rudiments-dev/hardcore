package dev.rudiments.app

import dev.rudiments.hardcore.file.{Dir, FileAdapter, TextFile, UnknownFile}
import dev.rudiments.hardcore.http.ThingEncoder
import dev.rudiments.hardcore.{All, Create, ID, Space, Thing, Type, Volatile}
import io.circe.{Encoder, Json}

object FileEncoder {
  def init(implicit space: Space): Unit = {
    Type.build[FileAdapter]

    ThingEncoder.path << Create(ID("Dir"), Volatile(All, Encoder.instance[Thing] {
      case dir: Dir => Json.obj(
        "type" -> Json.fromString("dir"),
        "path" -> Json.fromString(dir.absolutePath),
        "inside" -> Json.fromInt(dir.cache.state.size),
        "recursive" -> Json.fromLong(dir.totalRecursive)
      )
    }))

    ThingEncoder.path << Create(ID("TextFile"), Volatile(All, Encoder.instance[Thing] {
      case txt: TextFile => Json.obj(
        "type" -> Json.fromString("txt"),
        "path" -> Json.fromString(txt.absolutePath),
        "content" -> Json.arr(txt.cache.map(s => Json.fromString(s)):_*)
      )
    }))

    ThingEncoder.path << Create(ID("UnknownFile"), Volatile(All, Encoder.instance[Thing] {
      case unk: UnknownFile => Json.obj(
        "type" -> Json.fromString("unknown"),
        "path" -> Json.fromString(unk.absolutePath),
        "contentSize" -> Json.fromInt(unk.cache.size)
      )
    }))
  }
}
