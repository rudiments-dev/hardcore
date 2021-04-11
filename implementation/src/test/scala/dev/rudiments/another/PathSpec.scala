package dev.rudiments.another

import dev.rudiments.another.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class PathSpec extends AnyWordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  )

  case class Sample(
    id: String,
    reference: Long,
    name: String
  )

  case class Simple(key: String, value: Any)
  
  private val id1 = ID[Example](Seq(42L))
  private val id2 = ID[Sample](Seq("twenty four"))
  private val id3 = ID[Simple](Seq("forty two"))
  private val path = Path(Seq(id1, id2, id3))

  "Path represents IDs" in {
    path.toMap should be (Map(
      "Sample" -> id2,
      "Simple" -> id3,
      "Example" -> id1
    ))
  }

  "Path can be sliced with tail" in {
    path.after[Example] should be (Path(Seq(ID[Sample](Seq("twenty four")), ID[Simple](Seq("forty two")))))
    path.after[Sample] should be (Path(Seq(ID[Simple](Seq("forty two")))))
  }

  "Path can be sliced to the end" in {
    path.after[Simple] should be (EmptyPath)
  }
}
