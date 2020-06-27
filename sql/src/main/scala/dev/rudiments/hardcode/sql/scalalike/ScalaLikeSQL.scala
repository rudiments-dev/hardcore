package dev.rudiments.hardcode.sql.scalalike

import dev.rudiments.data.Batch.AllDeleted
import dev.rudiments.hardcode.sql.SQL
import dev.rudiments.hardcode.sql.materializer.Binding
import dev.rudiments.data.DataEvent
import dev.rudiments.data.ReadOnly.{Found, FoundAll, NotFound}
import dev.rudiments.data.CRUD.{AlreadyExists, Created, Deleted, FailedToCreate, FailedToDelete, FailedToUpdate, Updated}
import dev.rudiments.hardcore.types.{ID, Instance, Type}
import scalikejdbc.{DBSession, NoExtractor}

import scala.util.{Failure, Success, Try}


trait ScalaLikeSQL extends SQL[ScalaLikeTransaction] {

  val rawSQL: String
  val bindings: Set[Binding]

  def sql(): scalikejdbc.SQL[Nothing, NoExtractor] = {
    scalikejdbc.SQL(rawSQL).bindByName(bindings.map { case Binding(key, value) =>
      Symbol(key) -> value
    }.toSeq: _*)
  }
}

case class FindByIDSQL(
                    override val rawSQL: String,
                    override val bindings: Set[Binding],
                    override val softType: Type,
                    id: ID) extends ScalaLikeSQL {

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
    implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
    sql().map { rs =>
      softType.construct(rs.toMap().values.toSeq :_*) //todo refactor to Map
    }.single().apply() match {
      case Some(value) => Found(id, value)
      case None => NotFound(id)
    }
  }
}


case class CreateSQL(
                        override val rawSQL: String,
                        override val bindings: Set[Binding],
                        override val softType: Type,
                        checkSql: FindByIDSQL,
                        instance: Instance
                    ) extends ScalaLikeSQL {

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
      checkSql.exec(transaction) match {
        case NotFound(id) =>
          implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
          Try {
            sql().update().apply()
          } match {
            case _ => checkSql.exec(transaction) match {
              case Found(id, created) => Created(id, created)
              case NotFound(id) => FailedToCreate(id, instance)
            }
          }
        case Found(id, value) => AlreadyExists(id, value)
      }
  }
}

case class DropSQL(
                      override val rawSQL: String,
                      override val bindings: Set[Binding],
                      override val softType: Type,
                      checkSql: FindByIDSQL) extends ScalaLikeSQL {

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
    checkSql.exec(transaction) match {
      case event@NotFound(id) => event
      case Found(id, value) =>
        implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
        Try {
          sql().update().apply()
        } match {
          case Success(_) => checkSql.exec(transaction) match {
            case Found(id, instance) => FailedToDelete(id, instance)
            case NotFound(id) => Deleted(id, value)
          }
          case Failure(exception) => FailedToDelete(id, value)
        }
    }
  }
}

case class DropAllSQL(
                    override val rawSQL: String,
                    override val softType: Type) extends ScalaLikeSQL {

  override val bindings: Set[Binding] = Set.empty

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
    implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
    Try {
      sql().update().apply()
    } match {
      case Success(_) => AllDeleted
      case Failure(exception) => ???
    }
  }

}

case class UpdateSQL(
                    override val rawSQL: String,
                    override val bindings: Set[Binding],
                    override val softType: Type,
                    checkSql: FindByIDSQL) extends ScalaLikeSQL {

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
    checkSql.exec(transaction) match {
      case event@NotFound(id) => event
      case Found(id, old) =>
        implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
        Try {
          sql().update().apply()
        } match {
          case Success(_) => checkSql.exec(transaction) match {
            case Found(id, updated) => Updated(id, old, updated)
            case NotFound(id) => FailedToUpdate(id, old)
          }
          case Failure(exception) => FailedToUpdate(id, old)
        }
    }
  }
}

case class QuerySQL(
                     override val rawSQL: String,
                     override val bindings: Set[Binding],
                     override val softType: Type
                   ) extends ScalaLikeSQL {

  override def exec(transaction: ScalaLikeTransaction): DataEvent = {
    implicit val session: DBSession = transaction.dbCon.withinTxSession(transaction.underlying)
    Try {
      sql().map { rs =>
        softType.construct(rs.toMap().values.toSeq :_*) //todo refactor to Map
      }.list().apply()
    } match {
      case Failure(exception) => throw exception //todo error data command
      case Success(value) => FoundAll(value)
    }
  }
}