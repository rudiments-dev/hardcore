package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeSystemSpec extends AnyWordSpec with Matchers {
  private val mem = new Memory()
  private val tNode = mem /! Initial.types

  "can make TypeSystem from /types node" in {
    val ts = new TypeSystem(tNode)
    ts.types.size should be (27)
    ts.noThings.size should be (14)
  }

  "can seal type system" in {
    val ts = new TypeSystem(tNode)
    ts.seal().size should be (40)
  }
}
