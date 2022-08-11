package test.dev.rudiments.hardcore.file

import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileSpec extends AnyWordSpec with Matchers {
  private val files = ID("files")
  private val fileAgent = new FileAgent("src/test/resources/file-test")
  private val ctx: Memory = new Memory

  ctx += files -> Node.empty

  private val initialFound = ctx ?? Root match {
    case Found(All, values) =>
      values
    case _ => fail("Can't read initial memory state")
  }

  private val commitEvents: Map[Location, CRUD.Evt] = Map(
    Root -> Created(Data(Folder.typeOf, Map(
      ID("folder1") -> File.folder,
      ID("24.bin") -> File.unknownFile,
      ID("42.json") -> File.textFile
    ))),

    ID("folder1") -> Created(Data(Folder.typeOf, Map(ID("folder2") -> File.folder))),
    ID("24.bin") -> Created(Data(Binary, Seq[Byte](117, 110, 107, 110, 111, 119, 110, 32, 102, 105, 108, 101, 32, 101, 120, 97, 109, 112, 108, 101))),
    ID("42.json") -> Created(Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}"))),

    ID("folder1") / ID("folder2") -> Created(Data(Folder.typeOf, Map(ID("123.json") -> File.textFile))),
    ID("folder1") / ID("folder2") / ID("123.json") -> Created(Data(TextFile.typeOf, Seq("{", "  \"b\": false", "}"))),
  )
  private val commitMemory: Node = Node(
    Data(Folder.typeOf, Map(
      ID("folder1") -> File.folder,
      ID("24.bin") -> File.unknownFile,
      ID("42.json") -> File.textFile
    )),
    Map(
      ID("24.bin") -> Data(Binary, Seq[Byte](117, 110, 107, 110, 111, 119, 110, 32, 102, 105, 108, 101, 32, 101, 120, 97, 109, 112, 108, 101)),
      ID("42.json") -> Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}")),
    ),
    Map(
      ID("folder1") -> Node(
        Data(Folder.typeOf, Map(ID("folder2") -> File.folder)),
        Map.empty[ID, Thing],
        Map(
          ID("folder2") -> Node(
            Data(Folder.typeOf, Map(ID("123.json") -> File.textFile)),
            Map(
              ID("123.json") -> Data(TextFile.typeOf, Seq("{", "  \"b\": false", "}"))
            )
          )
        )
      )
    )
  )

  "can read file in Agent" in {
    val readen = fileAgent.read(Root)
    readen should be (
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

  "can prepare commit with memory" in {
    val loaded = fileAgent.compose(Root)
    loaded should be(commitMemory)
  }

  "can prepare commit via Agent" in {
    val out = fileAgent.reconsFor(ctx /! files)
    out should be(Prepared(Commit(commitEvents)))
  }

  "can save prepared into Context" in {
    val out = fileAgent.reconsFor(ctx /! files)
    out match {
      case Prepared(cmt) =>
        val result = ctx /! files << cmt
        result should be (Committed(cmt))
      case _ => fail("Unexpected result of load")
    }

    val committedData = commitEvents.map {
      case (l, Created(data)) => files / l -> data
      case (l, Updated(_, data)) => files / l -> data
      case (l, other) => fail(s"Unexpected: $other")
    }
    val cmt = Commit(commitEvents.map { case (k, v) => files / k -> v })
    val commit = Memory.commits / ID(cmt.hashCode().toString) -> cmt
    val shouldBe = committedData + commit

    val found = ctx ?? Root match {
      case Found(All, values) => values
      case other => fail("expecting Found All")
    }

    val diff = found -- initialFound.keys

    diff should be (committedData) //TODO refine file manipulation from internal memory work
  }

  "can write Commit into files elsewhere" in {
    val otherFile = new FileAgent("build/tmp/test-files")
    val node = Node.fromMap(commitEvents.toMap[Location, CRUD.O])
    otherFile.writeFileFromNode(node, Root) should be (WrittenTextFile(Data.empty))
  }

  "can write commit into json file" in {
    val otherFile = new FileAgent("build/tmp/test-files")
    ctx ? Memory.commits match {
      case Readen(node: Node) => otherFile.writeFileFromNode(node, Root)
      case other => fail("Expecting initial commit")
    }
  }
}
