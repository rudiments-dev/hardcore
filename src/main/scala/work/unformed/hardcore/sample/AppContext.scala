package work.unformed.hardcore.sample

import akka.http.scaladsl.server.Directive1
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.http.CrudRouter
import work.unformed.hardcore.repo.memory.MemoryRepo

object AppContext {
  implicit val itemMeta = Meta[Item](item => ID(item.id))
  val itemRepo = new MemoryRepo[Item]

  import akka.http.scaladsl.server.Directives._
  val itemDirective: Directive1[ID[Item]] = path(LongNumber).map(l => ID[Item, Long](l))

  import work.unformed.hardcore.http.CirceSupport._
  val itemRouter = new CrudRouter[Item]("item", itemRepo, itemDirective)
}

case class Item(
  id: Long,
  name: String
)
