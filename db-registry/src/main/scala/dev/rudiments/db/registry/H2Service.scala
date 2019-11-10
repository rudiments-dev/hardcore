package dev.rudiments.db.registry

import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, Find, Found}
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.types.{ID, Type}

class H2Service(adapter: H2Adapter, persistent: DataMemoryAdapter[Schema]) extends Service[SchemaCommand, SchemaEvent] {
  override def isDefinedAt(cmd: SchemaCommand): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: SchemaCommand): SchemaEvent = f(cmd)

  implicit val t: Type[Schema] = Type[Schema]
  val f: Skill[SchemaCommand, SchemaEvent] = {
    case ReadSchema(schemaName) =>
      persistent(Create(ID(schemaName), discoverSchema(schemaName)))
      persistent(Find(ID(schemaName))) match {
        case Found(_, value) => SchemaFound(value)
      }
  }

  private def discoverSchema(name: String): Schema = {
    adapter(DiscoverSchema(name)) match {
      case SchemaDiscovered(schemaName, tableNames) =>
        Schema(
          schemaName,
          tableNames
            .map(n => discoverTable(n, schemaName))
        )
      case ConnectionFailure(e) => throw e
    }
  }

  private def discoverTable(tableName: String, schemaName: String): Table = {
    adapter(DiscoverTable(tableName, schemaName)) match {
      case TableDiscovered(tableName, columns) =>
        Table(
          tableName,
          columns,
          columns
            .filter(c => c.pk)
        )
      case ConnectionFailure(e) => throw e
    }
  }
}

sealed trait SchemaCommand extends Command
case class ReadSchema(name: String) extends SchemaCommand

sealed trait SchemaEvent extends Event
case class SchemaFound(schema: Schema) extends SchemaEvent

sealed trait SchemaError extends SchemaEvent with Error
case class Failed(e: Throwable) extends SchemaError