package work.unformed.hardcore.sample

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.http.{CrudRouter, IDPath}
import work.unformed.hardcore.repo.EventStreamer
import work.unformed.hardcore.repo.memory.MemoryRepo

import scala.concurrent.ExecutionContext

object AppContext {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()
  implicit val itemMeta: Meta[Item] = Meta[Item](item => ID(item.id))
  implicit val eventMaster = new EventStreamer()

  val itemRepo = new MemoryRepo[Item]

  import work.unformed.hardcore.http.CirceSupport._
  val itemRouter = new CrudRouter[Item]("item", itemRepo, IDPath[Item, Long])
}

case class Item(
  id: Long,
  name: String
)
