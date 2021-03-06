package dev.rudiments.db.registry

import dev.rudiments.data._
import dev.rudiments.domain.{Domain, ID, Spec}
import dev.rudiments.hardcode.sql.schema.{FK, Table}
import dev.rudiments.hardcore.{Ask, Command, Error, Event, Reply, Service, Skill}

class H2Service(adapter: H2Adapter, state: State) extends Service[SchemaCommand, SchemaEvent] {
  override def isDefinedAt(cmd: Ask): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Ask): Reply = f(cmd)

  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Schema]
  val f: Skill = {
    case ReadSchema(schemaName) =>
      state(
        Create(
          ID(schemaName),
          t.fromProduct(domain, discoverSchema(schemaName))
        )
      )
      findSchema(schemaName)

    case FindSchema(schemaName) =>
      findSchema(schemaName)
  }

  private def discoverSchema(name: String): Schema = {
    adapter(DiscoverSchema(name)) match {
      case SchemaDiscovered(schemaName, tableNames) =>
        val tables = tableNames.map(n => n -> discoverTable(n, schemaName)).toMap
        Schema(
          schemaName,
          tables.values.toSet,
          discoverReferences(schemaName, tables)
        )
      case ConnectionFailure(e) => throw e
    }
  }

  private def discoverTable(tableName: String, schemaName: String): Table = {
    adapter(DiscoverTable(tableName, schemaName)) match {
      case TableDiscovered(tableName, columns) =>
        Table(
          tableName,
          columns
        )
      case ConnectionFailure(e) => throw e
    }
  }

  private def discoverReferences(schemaName: String, tables: Map[String, Table]): Set[FK] = {
    adapter(DiscoverReferences(schemaName)) match {
      case ReferencesDiscovered(_, references) =>
        references.map { r =>
          val table = tables(r.table)
          val ref = tables(r.refTable)
          val columns = r.columns.flatMap(c => table.columns.find(_.name == c))
          val refColumns = r.columns.flatMap(c => ref.columns.find(_.name == c))
          FK(
            table,
            ref,
            columns.zip(refColumns).toMap
          )
        }
      case ConnectionFailure(e) => throw e
    }
  }

  private def findSchema(schemaName: String): SchemaEvent = {
    state(Find(ID(schemaName))) match {
      case Found(_, value) => SchemaFound(
        Schema(
          value.extract("name"),
          value.extract("tables"),
          value.extract("references")
        )
      )
      case NotFound(_) => SchemaNotFound(schemaName)
    }
  }
}

sealed trait SchemaCommand extends Command
case class ReadSchema(name: String) extends SchemaCommand
case class FindSchema(name: String) extends SchemaCommand

sealed trait SchemaEvent extends Event
case class SchemaFound(schema: Schema) extends SchemaEvent

sealed trait SchemaError extends SchemaEvent with Error
case class Failed(e: Throwable) extends SchemaError
case class SchemaNotFound(name: String) extends SchemaError