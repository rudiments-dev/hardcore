package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.ReadOnly.{Count, Counted}
import dev.rudiments.hardcode.sql.SQLParts.From
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.Spec
import scalikejdbc.{DBSession, SQL}

class CountAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[Count, Counted] {

  override def apply(command: Count): Counted = {
    implicit val s = session
    val table = schema.tables(spec)

    SQL(
      s"""
         |SELECT COUNT(1) as cnt
         |FROM ${fromPart(From(schema, table, None))}
         |""".stripMargin
    ).map(rs => Counted(rs.get("cnt"))).single().apply().get
  }

}
