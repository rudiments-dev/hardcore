package dev.rudiments.hardcode.sql

import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.skills._
import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.{Adapter, Command, Result}
import dev.rudiments.data.ReadOnly.{Find, FindAll}
import dev.rudiments.data.CRUD._
import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.data.Batch._
import dev.rudiments.hardcore.types.Type
import scalikejdbc.DBSession

class SQLAdapter(schema: TypedSchema, session: DBSession)(implicit t: Type) extends Adapter[DataCommand, DataEvent] {

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = f(cmd)

  val f: DataSkill =
    List(
      new CreateAllSkill(schema, t)(session),
      new CreateSkill(schema, t)(session),
      new DeleteAllSkill(schema, t)(session),
      new DeleteSkill(schema, t)(session),
      new FindSkill(schema, t)(session),
      new ReplaceAllSkill(schema, t)(session),
      new UpdateSkill(schema, t)(session)
    ).foldLeft(new QuerySkill(schema, t)(session).asInstanceOf[DataSkill])(_ orElse _)

}


