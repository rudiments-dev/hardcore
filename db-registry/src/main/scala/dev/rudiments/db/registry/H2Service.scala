package dev.rudiments.db.registry

import dev.rudiments.data.SoftCache
import dev.rudiments.hardcode.sql.schema.{FK, Table}
import dev.rudiments.hardcore.{Command, Error, Event, Result, Service, Skill}
import dev.rudiments.data.CRUD.Create
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain.{ID, Spec, Domain}

class H2Service(adapter: H2Adapter, persistent: SoftCache) extends Service[SchemaCommand, SchemaEvent] {
  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Command): Result[SchemaEvent] = f(cmd)

  private implicit val domain: Domain = Domain()
  private implicit val t: Spec = domain.makeFromScala[Spec, Schema]
  val f: Skill[SchemaEvent] = {
    case ReadSchema(schemaName) =>
      persistent(
        Create(
          ID(schemaName),
          t.fromProduct(domain, discoverSchema(schemaName))
        )
      )
      findSchema(schemaName).toEither

    case FindSchema(schemaName) =>
      findSchema(schemaName).toEither
  }

  private def discoverSchema(name: String): Schema = {
    adapter(DiscoverSchema(name)) match {
      case Right(SchemaDiscovered(schemaName, tableNames)) =>
        val tables = tableNames.map(n => n -> discoverTable(n, schemaName)).toMap
        Schema(
          schemaName,
          tables.values.toSet,
          discoverReferences(schemaName, tables)
        )
      case Left(ConnectionFailure(e)) => throw e
    }
  }

  private def discoverTable(tableName: String, schemaName: String): Table = {
    adapter(DiscoverTable(tableName, schemaName)) match {
      case Right(TableDiscovered(tableName, columns)) =>
        Table(
          tableName,
          columns
        )
      case Left(ConnectionFailure(e)) => throw e
    }
  }

  private def discoverReferences(schemaName: String, tables: Map[String, Table]): Set[FK] = {
    adapter(DiscoverReferences(schemaName)) match {
      case Right(ReferencesDiscovered(_, references)) =>
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
      case Left(ConnectionFailure(e)) => throw e
    }
  }

  private def findSchema(schemaName: String): SchemaEvent = {
    persistent(Find(ID(schemaName))) match {
      case Right(Found(_, value)) => SchemaFound(
        Schema(
          value.extract("name"),
          value.extract("tables"),
          value.extract("references")
        )
      )
      case Left(NotFound(_)) => SchemaNotFound(schemaName)
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