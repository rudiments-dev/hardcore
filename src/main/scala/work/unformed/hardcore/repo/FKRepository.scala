package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._

trait FKRepository[R, A] extends Repository [A] {}

trait FKReadRepo[R, A] extends FKRepository[R, A] {
  def get(id: ID[R]): Either[Error[A], FKResult[R, A]]
  def find(filters: Filter[A]*): (ID[R], Iterable[A]) // TODO replace with commands
  def count(filters: Filter[A]*): Long
}


trait FKWriteRepo[R, A] extends FKReadRepo[R, A] {
  def create(ref: ID[R], values: Iterable[A]): Either[Error[A], FKCreated[R, A]]
  def update(ref: ID[R], values: Iterable[A]): Either[Error[A], FKUpdated[R, A]]
  def delete(ref: ID[R]): Either[Error[A], FKDeleted[R, A]]
  def createAll(values: Map[ID[R], Iterable[A]]): Either[Error[A], FKBatchCreated[R, A]]
  def deleteAll(): Either[Error[A], FKAllDeleted[R, A]]
}

trait BackRefRepo[R, A] extends Repository[A] {

}
