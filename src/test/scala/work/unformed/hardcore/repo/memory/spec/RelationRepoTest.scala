package work.unformed.hardcore.repo.memory.spec

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.{RouteTest, ScalatestRouteTest}
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.EventStreamer
import work.unformed.hardcore.repo.memory.{MemoryFKRepo, MemoryRepo}

import scala.concurrent.ExecutionContext
import scala.util.Random


class RelationRepoTest extends WordSpec with Matchers with LazyLogging {

  case class Container
  (
    id: Long,
    name: String,
    parts: Set[Parts] = Set.empty
  ) {
    def plain: Container = this.copy(parts = Set.empty)
  }


  case class Parts
  (
    comment: String
  )
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  implicit val eventStreamer: EventStreamer = new EventStreamer()


  implicit val containerMeta: Meta[Container] = Meta(value => ID(value.id))
  implicit val partMeta: Meta[Parts] = Meta(value => ID(value.comment))

  val containerRepo = new MemoryRepo[Container] {
    override val onEvent: PartialFunction[Event[_], Unit] = {
      case Deleted(_, deleted: Parts) =>
        val affected = handle(ReadAll()).asInstanceOf[ResultAll[Container]].values.filter(_.parts.contains(deleted))
        affected.foreach { element =>
          handle(Update(element.copy(parts = element.parts - deleted)))
        }
      case Updated(_, oldValue: Parts, newValue: Parts) =>
        val affected = handle(ReadAll()).asInstanceOf[ResultAll[Container]].values.filter(_.parts.contains(oldValue))
        affected.foreach { element =>
          handle(Update(element.copy(parts = element.parts - oldValue + newValue)))
        }
      case event => logger.info(s"Msg: $event")
    }
  }

  val partsRepo = new MemoryRepo[Parts]

  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "put collection into repository" in {
    partsRepo.handle(Create(Parts("123")))
    partsRepo.handle(Create(Parts("456")))

    containerRepo.handle(Create(sample))

    partsRepo.handle(Delete(Parts("123").identify))

    Thread.sleep(10000)

    val result = containerRepo.handle(Read(id))

    val b = 3
//    repo.get(id).unsafeRunSync() should be (Result(id, Container(42, "sample", Set(Parts("123"), Parts("456")))))
  }


}
