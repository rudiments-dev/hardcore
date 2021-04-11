package dev.rudiments.another.sql

import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.another.hardcore._
import dev.rudiments.another.{In, LogTx, Out, Tx}
import scalikejdbc.{DB, DBSession, SQL}

class AutoDbTx extends LogTx with StrictLogging {
  val session: DBSession = DB.readOnlySession()
  logger.info("AutoTx initiated: {}", session)
}

class H2Adapter extends PF {
  val schemas = new State[Schema]

  private val skills: Seq[PF] = Seq(
    new SagaSkill[CheckConnection, AutoDbTx, ConnectionOk]({ _ =>
      try {
        SQL("SELECT 1+1").single()
        ConnectionOk()
      } catch {
        case e: Exception => ConnectionFailure(e)
      }
    }),
    new TxSkill[InspectDB, AutoDbTx, InspectedDB]({ (_, tx) =>
      try {
        implicit val session: DBSession = tx.session
        val schemaNames = SQL("SHOW SCHEMAS").map { rs =>
          rs.string("SCHEMA_NAME")
        }.toList().apply()

        schemaNames.foreach { s =>
          val schema = Schema(s)
          schemas(Create(ID(Seq(schema.name)), schema))
          val tableNames = SQL(s"SHOW TABLES FROM ${s.toUpperCase()}").map { rs2 =>
            rs2.string("TABLE_NAME")
          }.toList().apply()

          tableNames
            .foreach { t2 =>
            val columns = SQL(s"SHOW COLUMNS FROM ${t2.toUpperCase()} FROM ${s.toUpperCase()}")
              .map { rs3 =>
              Column(
                rs3.string("field"),
                ColumnTypes.valueOf(rs3.string("type")),
                rs3.string("null").equalsIgnoreCase("YES"),
                !rs3.string("default").equalsIgnoreCase("NULL"),
                rs3.string("key").equalsIgnoreCase("PRI")
              )
            }.toList().apply()

            schema.tables(Create(ID(Seq(t2)), Table(t2, columns)))
          }

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
              |""".stripMargin.trim).bind(schema.name).map { rs =>
            FK(
              rs.string("name"),
              TableRef(
                ID[Table](Seq(rs.string("table_name"))),
                rs.string("table_columns").split(",").map(i => ID[Column](Seq(i)))
              ),
              TableRef(
                ID[Table](Seq(rs.string("ref_name"))),
                rs.string("ref_columns").split(",").map(i => ID[Column](Seq(i)))
              )
            )
          }.toIterable().apply().map(i => ID[FK](Seq(i.name)).asInstanceOf[Identifier] -> i).toMap
          schema.references(CreateAll(references))
        }

        schemas(FindAll(All)).flatMap[FoundAll[Schema]] { s =>
          InspectedDB(s.content.keys.collect { case id: ID[Schema] => id }.toSet)
        }
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
case class InspectDB() extends H2Command

trait H2Event extends Out
case class ConnectionOk() extends H2Event
case class InspectedDB(schemas: Set[ID[Schema]]) extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event