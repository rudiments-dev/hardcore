package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{AlreadyExists, Create, Created, FailedToCreate}
import dev.rudiments.data.ReadOnly.{Counted, Find, Found, NotFound}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.Result
import dev.rudiments.domain.{Domain, Spec}
import scalikejdbc.{DBSession, SQL}
import cats.syntax.either._

class CreateAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Create, Created] {
  override def apply(command: Create): Result[Created] = {
    import command.{key, value}
    val table = schema.tables(spec)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(spec.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), value.extract[Any](field))
    }.toSeq)


    val fields: Seq[String] = entity.values.map(_.column.name)

    for {
      _ <- new FindAction(schema, domain, spec)(session)(Find(key)).map { found =>
        AlreadyExists(found.key, found.value)
      } match {
        case Left(_: NotFound) => Right(Counted(0))
        case l@Left(_) => l
        case Right(r) => Left(r)
      }
      _ = {
        val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.map(Binding.toScalaLikeSQL)

        SQL(
          s"""
             |INSERT INTO ${schema.name}.${table.name} (${fields.mkString(", ")})
             |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
             |""".stripMargin
        ).bindByName(bindings :_*).execute().apply()(session)
      }
      created <- new FindAction(schema, domain, spec)(session)(Find(key)).bimap(
        _ => FailedToCreate(key, value),
        found => Created(found.key, found.value)
      )
    } yield created
  }
}
