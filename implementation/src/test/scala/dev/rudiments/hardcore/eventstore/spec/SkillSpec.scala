package dev.rudiments.hardcore.eventstore.spec

import akka.actor.ActorSystem
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.ActorMemory
import dev.rudiments.hardcore.repo.memory.SyncMemoryRepo
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{AsyncFlatSpec, Matchers}


@RunWith(classOf[JUnitRunner])
class SkillSpec extends AsyncFlatSpec with Matchers with StrictLogging {
  private case class Example(
    id: Long,
    name: String
  )

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val meta: Meta[Example] = Meta(value => ID(value.id))
  private val repo: SyncMemoryRepo[Example] = new SyncMemoryRepo[Example]()
  private implicit val es: Memory = new ActorMemory

  private val skill = es.skill(repo.handle)

  private val sample = Example(42, "sample")
  private val id = sample.identify

  def check(name: String, command: Command, event: Event, count: Long): Unit = {
    it should name in {
      skill(command) should be (event)
      for {
        _ <- es.state(command).map(_ should be (Some(event)))
        r <- es.countEvents().map(_ should be (count))
      } yield r
    }
  }

  check("no element by ID", Read(id), NotFound(id), 1)
  check("put item into repository", Create(id, sample), Created(id, sample), 2)
  check("update item in repository", Update(id, Example(42, "test")), Updated(id, sample, Example(42, "test")), 3)
  check("return same result on same command", Create(id, sample), Created(id, sample), 3)
  check("delete item from repository", Delete(id), Deleted(id, Example(42, "test")), 4)

  it should "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      skill(Create(ID(i), Example(i, s"$i'th element"))) should be (Created(ID(i), Example(i, s"$i'th element")))
    }
    es.countEvents().map(_ should be (10004))
  }

  private val batch = (10001 to 200000).map(i => ID(i) -> Example(i, s"$i'th element")).toMap
  check("endure 190.000 batch", CreateAll(batch), AllCreated(batch), 10005)

  check("clear repository", DeleteAll[ID[Example], Example](), AllDeleted[ID[Example], Example](), 10006)

  it should "store all events and commands" in {
    es.commands().map { commands =>
      commands(Read(id)) should be (NotFound(id))
      commands(Create(id, sample)) should be (Created(id, sample))
      commands(Update(id, Example(42, "test"))) should be (Updated(id, sample, Example(42, "test")))
      commands(Delete(id)) should be (Deleted(id, Example(42, "test")))
      commands(CreateAll(batch)) should be (AllCreated(batch))
      commands(DeleteAll[ID[Example], Example]()) should be (AllDeleted[ID[Example], Example]())
      commands.size should be (10006)
    }
  }
}
