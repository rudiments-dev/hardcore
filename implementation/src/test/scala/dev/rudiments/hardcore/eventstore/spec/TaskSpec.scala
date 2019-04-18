package dev.rudiments.hardcore.eventstore.spec

import akka.actor.ActorSystem
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.{ActorEventStore, Task}
import dev.rudiments.hardcore.repo.memory.SyncMemoryRepo
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class TaskSpec extends AsyncFlatSpec with Matchers {
  private case class Example(
    id: Long,
    name: String
  )

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val meta: Meta[Example] = Meta(value => ID(value.id))
  private val repo: SyncMemoryRepo[Example] = new SyncMemoryRepo[Example]()
  private implicit val es: ActorEventStore = new ActorEventStore

  private val task = Task(repo.handle)

  private val sample = Example(42, "sample")
  private val id = sample.identify

  def check(name: String, command: Command, event: Event, count: Long): Unit = {
    it should name in {
      task(command) should be (event)
      es.state(command).map(_ should be (Some(Seq(event))))
      es.countEvents().map(_ should be (count))
    }
  }

  check("no element by ID", Read(id), NotFound(id), 1)
  check("put item into repository", Create(id, sample), Created(id, sample), 2)
  check("update item in repository", Update(id, Example(42, "test")), Updated(id, sample, Example(42, "test")), 3)
  check("return same result on same command", Create(id, sample), Created(id, sample), 3)
  check("delete item from repository", Delete(id), Deleted(id, Example(42, "test")), 4)

  it should "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      task(Create(ID(i), Example(i, s"$i'th element"))) should be (Created(ID(i), Example(i, s"$i'th element")))
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


  private case class DoSomething(a: Int) extends Command
  private case class DoneSomething(b: String) extends Event

  private val doing = Task {
    case c: DoSomething => DoneSomething(c.a.toString)
  }

  it should "fail if not defined" in {
    Future {
      an[MatchError] should be thrownBy task(DoSomething(42))
    }
  }

  it should "take custom command" in {
    val command = DoSomething(42)
    val event = DoneSomething("42")
    doing(command) should be (event)
    es.state(command).map(_ should be (Some(Seq(event))))
    es.countEvents().map(_ should be (10007))
  }

  it should "be composable" in {
    val composite = doing.orElse(task)

    val command1 = DoSomething(24)
    val event1 = DoneSomething("24")
    composite(command1) should be (event1)
    es.state(command1).map(_ should be (Some(Seq(event1))))
    es.countEvents().map(_ should be (10008))

    val command2 = Read(id)
    val event2 = NotFound(id)
    composite(command2) should be (event2)
    es.state(command2).map(_ should be (Some(Seq(event2))))
    es.countEvents().map(_ should be (10008))
  }

  case class Boring(work: String) extends Command
  case class AtLast(done: Int) extends Event

  private val boring = Task {
    case c: Boring =>
      Thread.sleep(1000)
      AtLast(c.work.length)
  }

  it should "execute task in the future" in {
    boring.future(Boring("Hello, World!")).map(_ should be (AtLast(13)))
  }

  it should "await long-running action" in {
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
    boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
  }
}
