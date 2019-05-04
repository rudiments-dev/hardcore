package dev.rudiments.hardcore.eventstore.spec

import akka.actor.ActorSystem
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.ActorEventStore
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class TaskCompositionSpec extends AsyncFlatSpec with Matchers with StrictLogging {

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val es: EventStore = new ActorEventStore

  private case class DoSomething(a: Int) extends Command
  private case class DoneSomething(b: String) extends Event
  private val doing = es.task { case c: DoSomething => DoneSomething(c.a.toString) }

  case class Boring(work: String) extends Command
  case class AtLast(done: Int) extends Event

  private var counter = 0
  private val boring = es.task {
    case c: Boring =>
      counter += 1
      Thread.sleep(100)
      AtLast(c.work.length)
  }

  private val composite = doing.orElse(boring)

  it should "fail if not defined" in {
    Future {
      an[MatchError] should be thrownBy boring(DoSomething(42))
      an[MatchError] should be thrownBy doing(Boring("sample"))
    }
  }

  it should "take proper command" in {
    val command = DoSomething(42)
    val event = DoneSomething("42")
    composite(command) should be (event)
    es.state(command).map(_ should be (Some(Seq(event))))
    es.countEvents().map(_ should be (1))
  }

  it should "be composable" in {
    val command1 = DoSomething(24)
    val event1 = DoneSomething("24")
    composite(command1) should be (event1)

    val command2 = Boring("composed")
    val event2 = AtLast(8)
    composite(command2) should be (event2)

    counter should be (1)

    for {
      _ <- es.state(command1).map(_ should be (Some(event1)))
      _ <- es.state(command2).map(_ should be (Some(event2)))
      d <- es.countEvents().map(_ should be (3))
    } yield d
  }

  it should "execute task in the future" in {
    boring.future(Boring("Hello, World!")).map { res =>
      counter should be (2)
      res should be (AtLast(13))
    }
  }

  it should "await long-running action but run them only once" in {
    for {
      _ <- boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
      _ <- boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
      _ <- boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
      _ <- boring.future(Boring("FooBarBaz")).map(_ should be (AtLast(9)))
      r <- boring.future(Boring("FooBarBaz")).map { res =>
        counter should be (3)
        res should be (AtLast(9))
      }
    } yield r
  }
}
