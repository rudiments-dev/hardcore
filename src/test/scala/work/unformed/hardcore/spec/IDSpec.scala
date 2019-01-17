package work.unformed.hardcore.spec

import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl.{ID, ID1, Meta}

case class Example(
  id: Long,
  name: String,
  comment: Option[String] = None
)

class IDSpec extends WordSpec with Matchers {
  val sample = Example(42, "sample")

  "explicit ID from instance field" in {
    val id: ID[Example] = ID(sample.id)
    id should be (ID1[Example, Long](42))
  }

  "explicit meta generates ID from instance" in {
    val meta = new Meta[Example] {
      override def identify(value: Example): ID[Example] = ID(value.id)
    }
    meta.identify(sample) should be (ID1[Example, Long](42))
  }

  "construct meta from function generate ID from instance" in {
    val meta: Meta[Example] = Meta(value => ID(value.id))
    meta.identify(sample) should be (ID1[Example, Long](42))
  }

  "construct meta from function, implicit ID from instance" in {
    import work.unformed.hardcore.dsl.ID._

    implicit val meta: Meta[Example] = Meta(value => ID(value.id))
    sample.identify should be (ID1[Example, Long](42))
  }
}
