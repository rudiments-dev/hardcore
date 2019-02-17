package work.unformed.hardcore.sample

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.http.{CrudRouter, IDPath}
import work.unformed.hardcore.repo.memory.MemoryRepo

object AppContext {
  implicit val itemMeta: Meta[Item] = Meta[Item](item => ID(item.id))
  val itemRepo = new MemoryRepo[Item]

  import work.unformed.hardcore.http.CirceSupport._
  val itemRouter = new CrudRouter[Item]("item", itemRepo, IDPath[Item, Long])
}

case class Item(
  id: Long,
  name: String
)
