package dev.rudiments.sample

import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.http.{CrudRouter, IDPath}
import dev.rudiments.hardcore.repo.memory.MemoryRepo

object AppContext {
  implicit val itemMeta: Meta[Item] = Meta[Item](item => ID(item.id))
  val itemRepo = new MemoryRepo[Item]

  import dev.rudiments.hardcore.http.CirceSupport._
  val itemRouter = new CrudRouter[Item]("item", itemRepo, IDPath[Item, Long])
}

case class Item(
  id: Long,
  name: String
)
