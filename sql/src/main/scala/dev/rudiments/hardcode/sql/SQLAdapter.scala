package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.interpritator.CommandToSqlTransformer
import dev.rudiments.hardcode.sql.scalalike.{ScalaLikeSQLMaterializer, ScalaLikeTransaction, ScalaLikeTransactionProvider}
import dev.rudiments.hardcode.sql.schema.Schema
import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.data.soft.ReadOnly.Find
import dev.rudiments.hardcore.data.soft.SoftCRUD._
import dev.rudiments.hardcore.data.soft.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.DB

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


