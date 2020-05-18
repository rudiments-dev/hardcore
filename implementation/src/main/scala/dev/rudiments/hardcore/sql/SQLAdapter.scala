package dev.rudiments.hardcore.sql

import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.data.soft.ReadOnly.Find
import dev.rudiments.hardcore.data.soft.SoftCRUD.{Create, Delete, Update}
import dev.rudiments.hardcore.data.soft.{Batch, DataCommand, DataEvent, DataSkill, ReadOnly, SoftCRUD}
import dev.rudiments.hardcore.sql.interpritator.CommandToSqlTransformer
import dev.rudiments.hardcore.sql.realization.scalalike.{ScalaLikeSQLMaterializer, ScalaLikeTransaction, ScalaLikeTransactionProvider}
import dev.rudiments.hardcore.sql.schema.Schema
import scalikejdbc.DB
import dev.rudiments.hardcore.types._

import scala.collection.parallel

class SQLAdapter(db: DB, schema: Schema)(implicit t: Type) extends Adapter[DataCommand, DataEvent] {

  private val sqlTransformer = new CommandToSqlTransformer(schema)
  private val sqlMaterializer = new ScalaLikeSQLMaterializer()
  private val transactionProvider = new ScalaLikeTransactionProvider(db)
  override def isDefinedAt(x: DataCommand): Boolean = f.isDefinedAt(x)
  override def apply(cmd: DataCommand): DataEvent = f(cmd)

  val f: DataSkill = {
    case command : Create => withTransaction(sqlMaterializer.insertSQL(sqlTransformer.createToInsertSql(command)).exec)
    case command : Find => withTransaction(sqlMaterializer.findByIdSQL(sqlTransformer.findToFindByIdSql(command)).exec)
    case command : Delete => withTransaction(sqlMaterializer.dropSQL(sqlTransformer.deleteToDropSql(command)).exec)
    case command : Update => withTransaction(sqlMaterializer.updateSQL(sqlTransformer.updateToUpdateSql(command)).exec)
  }

  def withTransaction[T](f: ScalaLikeTransaction => T): T = {
    val transaction = transactionProvider.transaction()
    val result = f(transaction)
    transaction.commit()
    result
  }
}


