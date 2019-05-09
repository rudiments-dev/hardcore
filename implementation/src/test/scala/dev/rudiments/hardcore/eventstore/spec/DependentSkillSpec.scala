package dev.rudiments.hardcore.eventstore.spec

import akka.actor.ActorSystem
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.ActorMemory
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{AsyncFlatSpec, Matchers}


@RunWith(classOf[JUnitRunner])
class DependentSkillSpec extends AsyncFlatSpec with Matchers with StrictLogging {
  private implicit val actorSystem: ActorSystem = ActorSystem()


  private case class DoSomething(a: Int) extends Command
  private case class DoneSomething(b: String) extends Event

  private case class Boring(work: String) extends Command
  private case class AtLast(done: Int, something: DoneSomething) extends Event
  private var counter = 0

  private implicit val es: Memory with SkillSet = new ActorMemory().withSkill {
    case c: DoSomething =>
      Thread.sleep(100)
      DoneSomething(c.a.toString)
  }.withDependency { case c: Boring => DoSomething(c.work.length + 42) } {
    case (c: Boring, d: DoneSomething) =>
      counter += 1
      Thread.sleep(100)
      AtLast(c.work.length, d)
  }


  it should "run dependent skill as usual" in {
    val command = DoSomething(42)
    val event = DoneSomething("42")
    es(command) should be (event)

    for {
      _ <- es.state(command).map(_ should be (Some(event)))
      d <- es.countEvents().map(_ should be (1))
    } yield d
  }

  it should "run dependent skill in future" in {
    val command = DoSomething(24)
    val event = DoneSomething("24")

    for {
      _ <- es.async(command).map(_ should be (event))
      _ <- es.state(command).map(_ should be (Some(event)))
      d <- es.countEvents().map(_ should be (2))
    } yield d
  }

  it should "run skill and get dependency from store" in {
    val command1 = DoSomething(50)
    val event1 = DoneSomething("50")
    es.sync(command1) should be (event1)

    val command2 = Boring("composed")
    val event2 = AtLast(8, DoneSomething("50"))
    es.sync(command2) should be (event2)

    counter should be (1)

    for {
      _ <- es.state(command1).map(_ should be (Some(event1)))
      _ <- es.state(command2).map(_ should be (Some(event2)))
      d <- es.countEvents().map(_ should be (4))
    } yield d
  }

  it should "ask to run dependent skill" in {
    val command = Boring("Hello, World!")
    val event = AtLast(13, DoneSomething("55"))

    val dependency = DoSomething(55)
    val resolved = DoneSomething("55")

    for {
      _ <- es.async(command).map { res =>
        counter should be (2)
        res should be (event)
      }
      _ <- es.state(command).map(_ should be (Some(event)))
      _ <- es.state(dependency).map(_ should be (Some(resolved)))
      d <- es.countEvents().map(_ should be (6))
    } yield d
  }

  it should "await long-running action but run them only once" in {
    for {
      _ <- es.async(Boring("FooBarBaz")).map(_ should be (AtLast(9, DoneSomething("51"))))
      _ <- es.async(Boring("FooBarBaz")).map(_ should be (AtLast(9, DoneSomething("51"))))
      _ <- es.async(Boring("FooBarBaz")).map(_ should be (AtLast(9, DoneSomething("51"))))
      _ <- es.async(Boring("FooBarBaz")).map(_ should be (AtLast(9, DoneSomething("51"))))
      _ <- es.async(Boring("FooBarBaz")).map { res =>
          counter should be (3)
          res should be (AtLast(9, DoneSomething("51")))
        }
      _ <- es.state(Boring("FooBarBaz")).map(_ should be (Some(AtLast(9, DoneSomething("51")))))
      _ <- es.state(DoSomething(51)).map(_ should be (Some(DoneSomething("51"))))
      r <- es.countEvents().map(_ should be (8))
    } yield r
  }
}
