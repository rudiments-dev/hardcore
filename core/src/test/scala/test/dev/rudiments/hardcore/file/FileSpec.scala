package test.dev.rudiments.hardcore.file

import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.{File, FileAgent, Folder, TextFile, UnknownFile}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileSpec extends AnyWordSpec with Matchers {
  private val fileAgent = new FileAgent("src/test/resources/file-test")
  private val mem: Memory = new Memory

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
    fileAgent.load(Root, mem) should be(Prepared(Commit(
      Map(
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
      ), null
    )))
  }

  "can save prepared into Memory" in {
    fileAgent.load(Root, mem) match {
      case Prepared(cmt) => mem << cmt should be (Committed(cmt))
      case _ => fail("Unexpected result of load")
    }

    mem << Find(All) should be (Found(All, Map(
      Root -> Data(Folder.typeOf, Map(
        ID("folder1") -> File.folder,
        ID("24.bin") -> File.unknownFile,
        ID("42.json") -> File.textFile
      )),

      ID("folder1") -> Data(Folder.typeOf, Map(ID("folder2") -> File.folder)),
      ID("24.bin") -> Data(Binary, Nothing),
      ID("42.json") -> Data(TextFile.typeOf, Seq("{", "  \"a\": true", "}")),

      ID("folder1") / ID("folder2") -> Data(Folder.typeOf, Map(ID("123.json") -> File.textFile)),
      ID("folder1") / ID("folder2") / ID("123.json") -> Data(TextFile.typeOf, Seq("{", "  \"b\": false", "}")),
    )))
  }
}
