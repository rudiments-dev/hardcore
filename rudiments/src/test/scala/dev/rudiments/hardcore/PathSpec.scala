package dev.rudiments.hardcore

import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

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
  
  private val id1: ID[Example] = ID[Example, Long](42L)
  private val id2: ID[Sample] = ID[Sample, String]("twenty four")
  private val id3: ID[Simple] = ID[Simple, String]("forty two")
  private val path: Path[Example] = Path(id1, id2, id3)

  "Path represents IDs" in {
    path.toMap should be (Map(
      "Sample" -> id2,
      "Simple" -> id3,
      "Example" -> id1
    ))
  }

  "Can build path" in {
    id2 / id3 / id1 should be (path)
    id2 / (id3 / id1) should be (path)
    (id2 / id3) / id1 should be (path)
  }
}
