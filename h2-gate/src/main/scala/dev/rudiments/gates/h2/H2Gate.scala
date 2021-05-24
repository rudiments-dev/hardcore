package dev.rudiments.gates.h2

import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore._
import scalikejdbc.{DB, DBSession, SQL}

class H2Gate(config: H2Config) extends Location[Schema, Schema] {
  val schemas: Store[Schema, Schema] = new Store[Schema, Schema]

  val f: PartialFunction[(In, Tx), Out] = {
    case (_: CheckConnection, tx: H2Tx) => try {
      SQL("SELECT 1+1").single()
      ConnectionOk()
    } catch {
      case e: Exception => ConnectionFailure(e)
    }
    case (d: CrudPlus[Schema, Schema], tx: Tx) => schemas(d, tx)
    case (cmd: InspectSchema, tx: H2Tx) =>
      implicit val session: DBSession = tx.session
      schemas(Read[Schema, Schema](ID[Schema, String](cmd.name)), tx) |> [Readen[Schema, Schema]] { evt =>
        val schema = evt.value
        val s = schema.name.toUpperCase()

        SQL(s"SHOW TABLES FROM $s").map { tableRow =>
          tableRow.string("TABLE_NAME")
        }.toList().apply().foreach { tableName =>
          val t = tableName.toUpperCase()
          val columns = SQL(s"SHOW COLUMNS FROM $t FROM $s")
            .map { columnRow =>
              Column(
                columnRow.string("field"),
                ColumnTypes.valueOf(columnRow.string("type")),
                columnRow.string("null").equalsIgnoreCase("YES"),
                !columnRow.string("default").equalsIgnoreCase("NULL"),
                columnRow.string("key").equalsIgnoreCase("PRI")
              )
            }.toList().apply()

          schema.tables(Upsert(ID[Table, String](t), Table(t, columns)), tx)
        }

        SQL(
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
              ID[Table, String](rs.string("table_name")),
              rs.string("table_columns").split(",").map(i => ID[Column, String](i))
            ),
            TableRef(
              ID[Table, String](rs.string("ref_name")),
              rs.string("ref_columns").split(",").map(i => ID[Column, String](i))
            )
          )
        }.toIterable().apply().foreach { fk =>
          schema.references(Upsert[FK, FK](ID[FK, String](fk.name), fk), tx)
        }

        schemas(Read[Schema, Schema](ID[Schema, String](cmd.name)), tx) |> [Readen[Schema, Schema]] { evt =>
          evt.value.tables(Find[Table, Table](All), tx) |> [Found[Table, Table]] { tables =>
            evt.value.references(Find[FK, FK](All), tx) |>[Found[FK, FK]] { refs =>
              InspectedSchema(evt.value.name, tables.content.values.toSeq, refs.content.values.toSeq)
            }
          }
        }
      }
    case (_: InspectDB, tx: H2Tx) =>
      implicit val session: DBSession = tx.session
      val schemaNames = SQL("SHOW SCHEMAS").map { rs =>
        rs.string("SCHEMA_NAME")
      }.toList().apply()
      schemaNames.foreach { name =>
        schemas(Upsert[Schema, Schema](ID[Schema, String](name), Schema(name)), tx) |> [Created[Schema, Schema]] { evt =>
          f(InspectSchema(evt.value.name), tx)
        } |> [Updated[Schema, Schema]] { evt =>
          f(InspectSchema(evt.newValue.name), tx)
        } |> [Readen[Schema, Schema]] { evt =>
          f(InspectSchema(evt.value.name), tx)
        }
      }
      InspectedDB(schemaNames.toSet.map(ID[Schema, String]))
  }

  def apply(in: In): Out = {
    val tx = new H2Tx
    try {
      f(in, tx)
    } finally {
      tx.session.close()
    }
  }
}

class H2Tx extends Tx with StrictLogging { //TODO make session an object inside story?
  val session: DBSession = DB.readOnlySession()
  logger.info("AutoTx initiated: {}", session)
}

trait H2Command extends In
case class CheckConnection() extends H2Command
case class InspectDB() extends H2Command
case class InspectSchema(name: String) extends H2Command

trait H2Event extends Out
case class ConnectionOk() extends H2Event
case class InspectedDB(schemas: Set[ID[Schema]]) extends H2Event
case class InspectedSchema(name: String, tables: Seq[Table], references: Seq[FK]) extends H2Event
case class ConnectionFailure(e: Exception) extends H2Event