package test.dev.rudiments.hardcore.file

import dev.rudiments.hardcore.CRUD.Evt
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileSpec extends AnyWordSpec with Matchers {
  private val filePath = "src/test/resources/file-test"
  private val outFilePath = "build/tmp/test-files"
  private val files = ID("files")
  private val fileAgent = new FileAgent(filePath)
  private val ctx: Memory = new Memory

  private val initialFound = ctx ??* Root match {
    case Found(_, values) =>
      values
    case _ => fail("Can't read initial memory state")
  }

  ctx += files -> Node.empty

  private val commitEvents: Map[Location, CRUD.Evt] = Map(
    Root -> Updated(Node.empty, Node(Data(Folder.typeOf, Map(
      ID("folder1") -> File.folder,
      ID("24.bin") -> File.unknownFile,
      ID("42.json") -> File.textFile
    )))),

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
    val loaded = fileAgent.everything(Root)
    val node = Node.fromMap(loaded)
    node should be(commitMemory)
  }

  "can read everything" in {
    val loaded = fileAgent.everything()
    loaded should be(Map(
      Root -> Node(Data(Folder.typeOf, Map(
        ID("folder1") -> File.folder,
        ID("24.bin") -> File.unknownFile,
        ID("42.json") -> File.textFile
      ))),

      ID("folder1") -> Node(Data(Folder.typeOf, Map(ID("folder2") -> File.folder))),
      ID("24.bin") -> Data(Binary, Seq[Byte](117, 110, 107, 110, 111, 119, 110, 32, 102, 105, 108, 101, 32, 101, 120, 97, 109, 112, 108, 101)),
      ID("42.json") -> Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}")),

      ID("folder1") / ID("folder2") -> Node(Data(Folder.typeOf, Map(ID("123.json") -> File.textFile))),
      ID("folder1") / ID("folder2") / ID("123.json") -> Data(TextFile.typeOf, Seq("{", "  \"b\": false", "}"))
    ))
  }

  "can prepare commit via Agent" in {
    val out = fileAgent.reconcile(ctx /! files)
    out.foreach { case (l, evt) =>
      withClue(s"location: $l") {
        commitEvents.get(l) match {
          case Some(found) =>
            evt should be (found)
          case None => fail("Not found")
        }
      }
    }
    out should be (commitEvents)
  }

  "can save prepared into Context" in {
    val out = fileAgent.reconcile(ctx /! files)
    val crud = out.collect { case (l, evt: Evt) => l -> evt }
    val cmt = Commit(crud)
    val result = ctx.remember(files, Committed(cmt))
    result should be (Committed(cmt))

    ctx ??* files match {
      case Found(_, values) =>
        val expecting = commitMemory.everything()
        values.keySet.toSeq.sorted(Location).foreach { k =>
          withClue(s"comparing location: $k") {
            values(k) should be(expecting(k))
          }
        }
      case other => fail("expecting Found All")
    }
  }

  "can write Commit into files elsewhere" in {
    val otherFile = new FileAgent(outFilePath)
    val node = Node.fromEventMap(commitEvents)
    otherFile.writeFileFromNode(node, Root) should be (WrittenTextFile(Data.empty))
  }

  "can write commit into json file" in {
    val otherFile = new FileAgent(outFilePath)
    ctx ? Memory.commits match {
      case Readen(node: Node) => otherFile.writeFileFromNode(node, Root)
      case other => fail("Expecting initial commit")
    }
  }
}
