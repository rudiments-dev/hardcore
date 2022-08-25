package test.dev.rudiments.hardcore.file

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

  private val initialFound = ctx ?** Root match {
    case Found(_, values) =>
      values
    case _ => fail("Can't read initial memory state")
  }

  ctx += files -> Node.empty

  private val commitEvents: Map[Location, CRUD.Evt] = Map(
    Root -> Created(Data(Folder.typeOf, Map(
      ID("folder1") -> File.folder,
      ID("24.bin") -> File.unknownFile,
      ID("42.json") -> File.textFile
    ))),

    ID("folder1") -> Created(Node(Data(Folder.typeOf, Map(ID("folder2") -> File.folder)))),
    ID("24.bin") -> Created(Data(Binary, Seq[Byte](117, 110, 107, 110, 111, 119, 110, 32, 102, 105, 108, 101, 32, 101, 120, 97, 109, 112, 108, 101))),
    ID("42.json") -> Created(Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}"))),

    ID("folder1") / ID("folder2") -> Created(Node(Data(Folder.typeOf, Map(ID("123.json") -> File.textFile)))),
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

  "can prepare commit via Agent" ignore {
    val out = fileAgent.reconsFor(ctx /! files)
    out match {
      case Prepared(Commit(events, _)) =>
        events.foreach { case (l, evt) =>
          withClue(s"location: $l") {
            commitEvents.get(l) match {
              case Some(found) =>
                evt should be (found)
              case None => fail("Not found")
            }
          }
        }
    }
    out should be(Prepared(Commit(commitEvents)))
  }

  "can save prepared into Context" ignore {
    val out = fileAgent.reconsFor(ctx /! files)
    out match {
      case Prepared(cmt) =>
        val result = ctx.remember(files, Committed(cmt))
        result should be (Committed(cmt))

        val committedData = commitEvents.map {
          case (Root, Created(data)) => files -> Node(data)
          case (l, Created(data)) => files / l -> data
          case (l, Updated(_, data)) => files / l -> data
          case (l, other) => fail(s"Unexpected: $other")
        }

        val c = Commit(commitEvents.map { case (k, v) => files / k -> v })

        ctx ?** Root match {
          case Found(_, values) =>
            val diff = values -- initialFound.keys
            val committed = Memory.commits / ID(c.hashCode().toString) -> c
            val shouldBe = committedData + committed

            shouldBe.keys.foreach { k =>
              withClue(s"comparing location: $k") {
                diff(k) should be (shouldBe(k))
              }
            }

            diff should be(shouldBe)
          case other => fail("expecting Found All")
        }
      case _ => fail("Unexpected result of load")
    }
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
