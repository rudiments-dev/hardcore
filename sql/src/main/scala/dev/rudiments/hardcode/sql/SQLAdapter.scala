package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.interpritator.CommandToSqlTransformer
import dev.rudiments.hardcode.sql.scalalike.{ScalaLikeSQLMaterializer, ScalaLikeTransaction, ScalaLikeTransactionProvider}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Adapter, Command, Result}
import dev.rudiments.data.ReadOnly.{Find, FindAll}
import dev.rudiments.data.CRUD._
import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.data.Batch._
import dev.rudiments.hardcore.types.Type
import scalikejdbc.DB

class SQLAdapter(db: DB, schema: TypedSchema)(implicit t: Type) extends Adapter[DataCommand, DataEvent] {

  private val sqlTransformer = new CommandToSqlTransformer(schema)
  private val sqlMaterializer = new ScalaLikeSQLMaterializer()
  private val transactionProvider = new ScalaLikeTransactionProvider(db)
  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = f(cmd)

  val f: DataSkill = {
    case command: Create => withTransaction(sqlMaterializer.insertSQL(sqlTransformer.createToInsertSql(command)).exec).toEither
    case command: Find => withTransaction(sqlMaterializer.findByIdSQL(sqlTransformer.findToFindByIdSql(command)).exec).toEither
    case command: Delete => withTransaction(sqlMaterializer.dropSQL(sqlTransformer.deleteToDropSql(command)).exec).toEither
    case command: Update => withTransaction(sqlMaterializer.updateSQL(sqlTransformer.updateToUpdateSql(command)).exec).toEither
    case command: FindAll => withTransaction(sqlMaterializer.querySQL(sqlTransformer.queryToSelectSql(command)).exec).toEither
    case command: CreateAll =>
      val commands = command.batch.map { case (id, instance) => Create(id, instance) }
        .map(sqlTransformer.createToInsertSql)
        .map(sqlMaterializer.insertSQL)
      val sql = SQL.sequence(commands.toSeq, {
        events: Seq[DataEvent] =>
          AllCreated(events.map {
            case Created(key, value) => key -> value
            case _ => ???
          }.toMap)
      })
      withTransaction(sql.exec).toEither
    case _: DeleteAll.type => withTransaction(sqlMaterializer.dropAllSQL(sqlTransformer.deleteAllDropSql(t)).exec).toEither
    case command : ReplaceAll =>
      withTransaction(transaction => {
        sqlMaterializer.dropAllSQL(sqlTransformer.deleteAllDropSql(t)).exec(transaction)
        val commands = command.batch.map { case (id, instance) => Create(id, instance) }
          .map(sqlTransformer.createToInsertSql)
          .map(sqlMaterializer.insertSQL)
        val sql = SQL.sequence(commands.toSeq, {
          events: Seq[DataEvent] =>
            AllReplaced(events.map {
              case Created(key, value) => key -> value
              case _ => ???
            }.toMap)
        })
        sql.exec(transaction).toEither
      })
  }

  def withTransaction[T](f: ScalaLikeTransaction => T): T = {
    val transaction = transactionProvider.transaction()
    val result = f(transaction)
    transaction.commit()
    result
  }
}


