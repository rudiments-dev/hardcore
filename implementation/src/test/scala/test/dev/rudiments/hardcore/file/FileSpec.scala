package test.dev.rudiments.hardcore.file

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.{File, FileAgent, Folder}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileSpec extends AnyWordSpec with Matchers {
  private val fileAgent = new FileAgent(".")

  "can read file in Agent" in {
    fileAgent.read(Root) match {
      case Readen(Data(_, values: Map[Location, File])) =>
        values.groupBy(_._2).size should be (3)
      case _ => fail("Expecting all kinds of files in WORKING_DIR")
    }
  }
}
