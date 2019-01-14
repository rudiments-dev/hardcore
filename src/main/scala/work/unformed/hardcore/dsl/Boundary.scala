package work.unformed.hardcore.dsl

trait Boundary {
  def add[DTO](dao: DAO[DTO]): Unit

  def make[DTO](): Unit

  def get[DTO]: DAO[DTO]

  //TODO add Tx
}

trait DAO[DTO]