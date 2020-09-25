package dev.rudiments.hardcode.sql

import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.actions._
import dev.rudiments.domain.{Domain, Spec}
import dev.rudiments.hardcore.{Adapter, Command, Result, Skill}
import scalikejdbc.DBSession

class SQLAdapter(schema: TypedSchema, domain: Domain, session: DBSession)(implicit spec: Spec) extends Adapter[DataCommand, DataEvent] {

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
  override def apply(cmd: Command): Result[DataEvent] = f(cmd)

  val f: DataSkill = Skill.fromActions[DataEvent](
    new CreateAllAction(schema, spec)(session),
    new CreateAction(schema, domain, spec)(session),
    new DeleteAllAction(schema, spec)(session),
    new DeleteAction(schema, domain, spec)(session),
    new FindAction(schema, domain, spec)(session),
    new ReplaceAllAction(schema, spec)(session),
    new UpdateAction(schema, domain, spec)(session),
    new QueryAction(schema, domain, spec)(session),
    new CountAction(schema, spec)(session)
  )

}


