package work.unformed.hardcore.spec

import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl.{ID, ID1}

case class Example(
  id: Long,
  name: String,
  comment: Option[String] = None
)

class IDSpec extends WordSpec with Matchers {
  "explicit ID from instance field" in {
    val sample = Example(42, "sample")
    val id = ID[Example, Long](sample.id)
    id should be (ID1[Example, Long](42))
  }
}
