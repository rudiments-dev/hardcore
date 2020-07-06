package dev.rudiments.hardcore.flow

import dev.rudiments.Defaults
import dev.rudiments.data.CRUD.{Create, Created}
import dev.rudiments.data.SoftCache
import dev.rudiments.types.{DTO, ID, Instance, Type, TypeSystem}
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class ControlFlowSpec extends WordSpec with Matchers {
  private case class Example(
    id: Long = Defaults.long,
    name: String
  ) extends DTO

  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private implicit val t: Type = typeSystem.asType[Example]

  private implicit val flow: ControlFlow = new ControlFlow()
  private val ctrl = new Controlled(new SoftCache)

  private val c1 = Create(ID(1L), Instance(1L, "one"))
  private val c2 = Create(ID(2L), Instance(2L, "two"))
  private val c3 = Create(ID(3L), Instance(3L, "three"))

  private val e1 = Created(ID(1L), Instance(1L, "one"))
  private val e2 = Created(ID(2L), Instance(2L, "two"))
  private val e3 = Created(ID(3L), Instance(3L, "three"))

  "contains no events if no executions" in {
    flow.memory should be (mutable.Queue.empty)
    flow.asMap should be (Map.empty)
    flow.lastMessage(c1) should be (None)
  }

  "control flow over atomic Skill saves pairs Command => Event" in {
    ctrl(c1)
    ctrl(c2)
    ctrl(c3)

    flow.memory should be (mutable.Queue(
      c1 -> InProgress, c1 -> e1,
      c2 -> InProgress, c2 -> e2,
      c3 -> InProgress, c3 -> e3
    ))

    flow.asMap should be (Map(
      c1 -> Seq(InProgress, e1),
      c2 -> Seq(InProgress, e2),
      c3 -> Seq(InProgress, e3),
    ))

    flow.lastMessage(c3) should be (Some(e3))
  }
}
