package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.ReadOnly.{Count, Counted}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, Where}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.types.Type
import scalikejdbc.{DBSession, SQL}

class CountAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[Count, Counted] {

  override def apply(command: Count): Result[Counted] = {
    implicit val s = session
    val table = schema.tables(t)

    SQL(
      s"""
         |SELECT COUNT(1) as cnt
         |FROM ${fromPart(From(schema, table, None))}
         |""".stripMargin
    ).map(rs => Counted(rs.get("cnt"))).single().apply().get.toEither
  }

}
