package test.dev.rudiments.git

import dev.rudiments.git.{Blob, Commit, Reader, Tree, Writer}
import dev.rudiments.utils.Log
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.nio.file.{Files, Path}

@RunWith(classOf[JUnitRunner])
class GitObjectTest extends AnyWordSpec with Matchers with Log {
  private val dir = Path.of("..").toAbsolutePath //TODO fix

  "git objects" ignore { //TODO read this objects from the repository
    "can read blob from repo" in {
      val readen = Reader.read(dir, "ce428f3bf04949f1386c964c5ef6c282861ec64a")
      readen match
        case Left(err) => throw err
        case Right(obj) => obj should be(Blob("dependencies {\n    implementation project(':core')\n    implementation project(':file')\n}\n"))
    }

    "can read tree from repo" in {
      val readen = Reader.read(dir, "a530c65ac6f8fc4323004876a159726f84c278b9")
      readen match
        case Left(err) => throw err
        case Right(obj) =>
          obj.header should be("tree 620")
    }

    "can write blob into repo" in {
      val someStuff = Blob("some stuff\n")
      Writer.deleteIfExist(dir, someStuff) should be(Writer.Status.Success)
      Writer.write(dir, someStuff) should be(Writer.Status.Success)
      Reader.read(dir, "b5fd817de972cdb092b7dfbeeb1bedb4f05eb218") should be(Right(someStuff))
    }

    "can write tree into repo" in {
      val someBlob = Blob("some test blob\n")
      Writer.deleteIfExist(dir, someBlob) should be(Writer.Status.Success)
      Writer.write(dir, someBlob) should be(Writer.Status.Success)

      val someTree = Tree(Seq(Tree.Item(Tree.Mode.File, "some_blob", someBlob.hash)))
      Writer.deleteIfExist(dir, someTree)
      Writer.write(dir, someTree)
      Reader.read(dir, "cd7641db93deb932638f99daa916b0e1d2d93e51") should be(Right(someTree))
    }

    "can read commit from repo" in {
      val readen = Reader.read(dir, "a3e9375e5ea70baf7d6a4ba343c59619aee1f2f0")
      readen match
        case Left(err) => throw err
        case Right(c) =>
          c.header should be("commit 238")
    }

    "can read parent and tree of commit" in {
      val result = for {
        first <- Reader.read(dir, "a3e9375e5ea70baf7d6a4ba343c59619aee1f2f0")
        tree <- Reader.read(dir, first.asInstanceOf[Commit].tree.toString)
        second <- Reader.read(dir, first.asInstanceOf[Commit].parent.head.toString)
      } yield (first, tree, second)

      result match {
        case Left(err) => throw err
        case Right(f, t, s) =>
          f.header should be("commit 238")
          t.header should be("tree 620")
          s.header should be("commit 275")
      }
    }
  }
}
