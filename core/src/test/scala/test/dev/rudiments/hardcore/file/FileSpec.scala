package test.dev.rudiments.hardcore.file

import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.{File, FileAgent, Folder, TextFile, UnknownFile, WrittenTextFile}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileSpec extends AnyWordSpec with Matchers {
  private val fileAgent = new FileAgent("src/test/resources/file-test", Root)
  private val ctx: Context = new Context

  private val initialFound = ctx ?? Root match {
    case Found(All, values) => values
    case _ => fail("Can't read initial memory state")
  }

  private val commitData: Map[Location, CRUD.Evt] = Map(
    Root -> Created(Data(Folder.typeOf, Map(
      ID("folder1") -> File.folder,
      ID("24.bin") -> File.unknownFile,
      ID("42.json") -> File.textFile
    ))),

    ID("folder1") -> Created(Data(Folder.typeOf, Map(ID("folder2") -> File.folder))),
    ID("24.bin") -> Created(Data(Binary, Nothing)),
    ID("42.json") -> Created(Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}"))),

    ID("folder1") / ID("folder2") -> Created(Data(Folder.typeOf, Map(ID("123.json") -> File.textFile))),
    ID("folder1") / ID("folder2") / ID("123.json") -> Created(Data(TextFile.typeOf, Seq("{", "  \"b\": false", "}"))),
  )

  "can read file in Agent" in {
    fileAgent.read(Root) should be (
      Readen(
        Data(
          Folder.typeOf,
          Map(
            ID("folder1") -> File.folder,
            ID("24.bin") -> File.unknownFile,
            ID("42.json") -> File.textFile
          )
        )
      )
    )
  }

  "can prepare commit via Agent" in {
    fileAgent.load(Root, ctx) should be(Prepared(Commit(commitData)))
  }

  "can save prepared into Context" in {
    fileAgent.load(Root, ctx) match {
      case Prepared(cmt) => ctx << cmt should be (Committed(cmt))
      case _ => fail("Unexpected result of load")
    }

    val committedData = commitData.view.mapValues {
      case c: Created => c.data
      case other => fail(s"Unexpected: $other")
    }.toMap
    val commit = Context.commits / ID("-599147518") -> Commit(commitData)
    val found = ctx ?? Root
    found should be (Found(All, initialFound ++ committedData + commit)) //TODO refine file manipulation from internal memory work
  }

  "can write Commit into files elsewhere" in {
    val otherFile = new FileAgent("build/tmp/test-files", Root)
    val node = Memory.fromMap(commitData.toMap[Location, CRUD.O])
    otherFile.writeFileFromNode(node, Root) should be (WrittenTextFile(Data.empty))

  }
}
