package dev.rudiments.hardcore.sql.realization.scalalike

import dev.rudiments.hardcore.data.soft.DataEvent
import dev.rudiments.hardcore.data.soft.ReadOnly.{Found, NotFound}
import dev.rudiments.hardcore.data.soft.SoftCRUD.{AlreadyExists, Created, Deleted, FailedToCreate, FailedToDelete, FailedToUpdate, Updated}
import dev.rudiments.hardcore.sql.SQL
import dev.rudiments.hardcore.sql.materializer.Binding
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
      softType.constructSoft(rs.toMap().values) //todo refactor to Map
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
            sql().execute().apply()
          } match {
            case _ => checkSql.exec(transaction) match {
              case Found(id, instance) => Created(id, instance)
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
          sql().execute().apply()
        } match {
          case _ => checkSql.exec(transaction) match {
            case Found(id, instance) => FailedToDelete(id, instance)
            case NotFound(id) => Deleted(id, value)
          }
        }
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
          sql().execute().apply()
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