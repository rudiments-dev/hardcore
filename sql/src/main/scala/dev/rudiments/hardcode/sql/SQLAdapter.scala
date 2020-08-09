package dev.rudiments.hardcode.sql

import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.actions._
import dev.rudiments.types.Type
import dev.rudiments.hardcore.{Adapter, Command, Result, Skill}
import scalikejdbc.DBSession

class SQLAdapter(schema: TypedSchema, session: DBSession)(implicit t: Type) extends Adapter[DataCommand, DataEvent] {

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = f(cmd)

  val f: DataSkill = Skill.fromActions[DataEvent](
    new CreateAllAction(schema, t)(session),
    new CreateAction(schema, t)(session),
    new DeleteAllAction(schema, t)(session),
    new DeleteAction(schema, t)(session),
    new FindAction(schema, t)(session),
    new ReplaceAllAction(schema, t)(session),
    new UpdateAction(schema, t)(session),
    new QueryAction(schema, t)(session),
    new CountAction(schema, t)(session)
  )

}


