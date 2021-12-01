package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.Smt

@RunWith(classOf[JUnitRunner])
class DataSpec extends AnyWordSpec with Matchers {
  private val data1: Data = Smt(42, "sample", Some("example")).asData
  private val data2 = Smt(13, "test", None).asData

  "can build primitive data" in {
    "test".asData should be (Data(Plain.Text(4), "test")) // or .build is not a good option?
  }

  "can build data instance with type" in {
    Data.build[Smt](42, "sample", Some("example")) should be (data1)
    Data.build[Smt](13, "test", None) should be (data2)
  }

  "Update required field" in {
    data1(Update(ID("name"), "updated".asData)) should be (Updated(ID("name"), "sample".asData, "updated".asData))
  }

  "Delete optional field" in {
    data1(Delete(ID("comment"))) should be (Deleted(ID("comment"), "example".asData))
  }

  "Create empty optional field" in {
    data2(Create(ID("comment"), "created".asData)) should be (Created(ID("comment"), "created".asData))
  }

  "can update required field" in {
    data1(Updated(ID("name"), "sample".asData, "updated".asData)) should be (Smt(42, "updated", Some("example")).asData)
  }

  "can delete optional field" in {
    data1(Deleted(ID("comment"), "sample".asData)) should be (Smt(42, "updated", None).asData)
  }

  "can create empty optional field" in {
    data2(Created(ID("comment"), "created".asData)) should be (Smt(13, "test", Some("created")).asData)
  }

  "can't delete required field" in {
    data1(Delete(ID("name"))) should be (Conflict(ID("name")))
    //TODO data(Deleted(ID("name), ???) should be (???)
  }

  "can't create existing optional field" in {
    data1(Create(ID("comment"), "failed".asData)) should be (AlreadyExist(ID("comment"), "example".asData))
    data1(Created(ID("comment"), "failed".asData)) should be (AlreadyExist(ID("comment"), "example".asData))
  }

  "can't delete empty optional filed" in {
    data2(Delete(ID("comment"))) should be (Conflict(ID("comment")))
    data2(Deleted(ID("comment"), "fail".asData)) should be (Conflict(ID("comment")))
  }
}
