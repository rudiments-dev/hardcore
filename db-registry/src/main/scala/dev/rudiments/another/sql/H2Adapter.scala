package dev.rudiments.another.sql

import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.another.{In, Out, Tx}
import dev.rudiments.another.hardcore.{CompositeSkill, Create, Find, Found, ID, PF, SagaSkill, State, TxSkill, Update}
import scalikejdbc.{DB, DBSession, SQL}

class AutoDbTx extends Tx with StrictLogging {
  val session: DBSession = DB.readOnlySession()
  logger.info("AutoTx initiated: {}", session)
}

class H2Adapter extends PF {

  //private val schemes = new State[Schema] - schema is too composite
  val tables = new State[Table]
  val refs = new State[FK] //TODO Map[ID, Set[ID]] for generic references and indexes

  private val skills: Seq[PF] = Seq(
    new SagaSkill[CheckConnection, AutoDbTx, ConnectionOk]({ in: CheckConnection =>
      try {
        SQL("SELECT 1+1").single()
        ConnectionOk()
      } catch {
        case e: Exception => ConnectionFailure(e)
      }
    }),
    new TxSkill[DiscoverSchema, AutoDbTx, SchemaDiscovered]({ (in: DiscoverSchema, tx: AutoDbTx) =>
      try {
        implicit val session: DBSession = tx.session
        val tables = SQL("SHOW TABLES FROM " + in.name).map { rs =>
          rs.string("table_name")
        }.toIterable().apply().toSet
        SchemaDiscovered(in.name, tables)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }
    }),
    new TxSkill[DiscoverTable, AutoDbTx, TableDiscovered]({(in: DiscoverTable, tx: AutoDbTx) =>
      try {
        implicit val session: DBSession = tx.session
        val columns = SQL("SHOW COLUMNS FROM " + in.tableName + " FROM " + in.schemaName).map { rs =>
          Column(
            rs.string("field"),
            ColumnTypes.valueOf(rs.string("type")),
            rs.string("null").equalsIgnoreCase("YES"),
            !rs.string("default").equalsIgnoreCase("NULL"),
            rs.string("key").equalsIgnoreCase("PRI")
          )
        }.toIterable().apply().toSeq
        tables(Create(ID(Seq(in.tableName)), Table(in.tableName, columns, Set.empty)))
        TableDiscovered(in.tableName, columns)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }
    }),
    new TxSkill[DiscoverReferences, AutoDbTx, ReferencesDiscovered]({(in: DiscoverReferences, tx: AutoDbTx) =>
      try {
        implicit val session: DBSession = tx.session
        val references = SQL(
          """
            |SELECT DISTINCT -- table_name (table_columns) REFERENCES ref_name (ref_columns)
            |    fk.constraint_name AS name,
            |    fk.table_name      AS table_name,
            |    fk.column_list     AS table_columns,
            |    pk.table_name      AS ref_name,
            |    pk.column_list     AS ref_columns
            |FROM   information_schema.constraints fk
            |  JOIN information_schema.constraints pk
            |      ON fk.unique_index_name =  pk.unique_index_name
            |     AND pk.constraint_type =    'PRIMARY KEY'
            |WHERE fk.table_schema = ?
            |  AND fk.constraint_type = 'REFERENTIAL'
            |""".stripMargin.trim).bind(in.schemaName).map { rs =>

          val fkRefs = rs.string("table_columns")
            .split(",")
            .map(i => ID[Column](Seq(i)))
            .zip(
              rs.string("ref_columns")
                .split(",")
                .map(i => ID[Column](Seq(i)))
            ).toMap

          FK(
            rs.string("name"),
            ID[Table](Seq(rs.string("table_name"))),
            ID[Table](Seq(rs.string("ref_name"))),
            fkRefs
          )
        }.toIterable().apply().toSet

        references.foreach { it =>
          refs(Create(ID(Seq(it.name)), it))

          tables(Find(it.from)).flatMap[Found[Table]] { found =>
            tables(Update(it.from, found.value.copy(references = found.value.references + it)))
          }
        }
        ReferencesDiscovered(in.schemaName, references)
      } catch {
        case e: Exception => ConnectionFailure(e)
      }
    })
  )

  private val composite = new CompositeSkill(skills)
  override val f: PartialFunction[(In, Tx), Out] = composite.f
  override val signature: Seq[(ID[In], ID[Out])] = composite.signature

  def apply[I <: In](in: I): Out = this.apply(in, new AutoDbTx)
}

trait H2Command extends In
case class CheckConnection() extends H2Command
case class DiscoverSchema(name: String) extends H2Command
case class DiscoverTable(tableName: String, schemaName: String) extends H2Command
case class DiscoverReferences(schemaName: String) extends H2Command

trait H2Event extends Out
case class ConnectionOk() extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event
case class SchemaDiscovered(name: String, tables: Set[String]) extends H2Event
case class TableDiscovered(name: String, columns: Seq[Column]) extends H2Event
case class ReferencesDiscovered(schemaName: String, references: Set[FK]) extends H2Event