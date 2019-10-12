package dev.rudiments.db.registry

import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, Find, Found}
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.types.{ID, Type}

class H2Service(adapter: H2Adapter, persistent: DataMemoryAdapter[Schema]) extends Service[SchemaCommand, SchemaEvent] {
  override def isDefinedAt(cmd: SchemaCommand): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: SchemaCommand): SchemaEvent = f(cmd)

  implicit val t: Type[Schema] = Type[Schema]
  val f: Skill[SchemaCommand, SchemaEvent] = {
    case ReadSchema(name) =>
      adapter(DiscoverSchema(name)) match {
        case SchemaDiscovered(name, tableNames) =>
          persistent(Create(ID(name), Schema(
            name,
            tableNames.map(n => adapter(DiscoverTable(n, name)) match {
              case TableDiscovered(tableName, columns) => Table(tableName, columns, columns.filter(_.pk))
              case ConnectionFailure(e) => throw e
            })
          )))
          persistent(Find(ID(name))) match {
            case Found(_, value) => SchemaFound(value)
          }
        case ConnectionFailure(e) => Failed(e)
      }
  }
}

sealed trait SchemaCommand extends Command
case class ReadSchema(name: String) extends SchemaCommand

sealed trait SchemaEvent extends Event
case class SchemaFound(schema: Schema) extends SchemaEvent

sealed trait SchemaError extends SchemaEvent with Error
case class Failed(e: Throwable) extends SchemaError