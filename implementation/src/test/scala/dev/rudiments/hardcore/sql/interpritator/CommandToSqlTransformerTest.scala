package dev.rudiments.hardcore.sql.interpritator

import dev.rudiments.hardcore.data.soft.SoftCRUD._
import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.predicates.{IntEquals, StringEquals}
import dev.rudiments.hardcore.sql._
import dev.rudiments.hardcore.sql.parts.{ColumnWhereExpression, Equals, From, Select, Selector, Where}
import dev.rudiments.hardcore.sql.schema.{Column, ColumnTypes, Schema, Table}
import dev.rudiments.hardcore.types.SoftID.SoftID1
import dev.rudiments.hardcore.types.{DTO, Field, FieldFlag, Infinity, NegativeInfinity, NumberFormat, PositiveInfinity, SoftID, Type, Types}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class CommandToSqlTransformerTest extends FlatSpec with Matchers {

  case class Foo(a: Int, b: String, d: Option[Int]) extends DTO

  val fooType: Type = Type("Foo", Map(
    "a" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Required),
    "b" -> Field(Types.Text(Infinity), FieldFlag.Required),
    "d" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Optional),
  ), Seq("a"))


  val aColumn: Column = Column("a", ColumnTypes.INT, nullable = false, default = false, pk = true)
  val bColumn: Column = Column("b", ColumnTypes.VARCHAR(150), nullable = false, default = false, pk = false)
  val dColumn: Column = Column("d", ColumnTypes.INT, nullable = true, default = false, pk = false)

  val table: Table = Table(
    "Foo",
    Seq(
      aColumn,
      bColumn,
      dColumn
    ),
    Seq(aColumn)
  )

  val schema: Schema = Schema(
    "dev",
    Map(
      fooType -> table
    ),
    Set.empty
  )

  it should "test select query generation" in {
    val query = Query(Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ), fooType)

    val result = new CommandToSqlTransformer(schema).queryToSelectSql(query)
    result should be(
      SelectSql(
        Select(Seq(
          Selector(aColumn, None),
          Selector(bColumn, None),
          Selector(dColumn, None)
        )),
        From(table, None),
        Where(
          Set(
            ColumnWhereExpression(bColumn, Equals("bay")),
            ColumnWhereExpression(aColumn, Equals(5))
          )
        )
      )
    )
  }


  it should "test query from create" in {
    val command = Create(SoftID1(5)(fooType), fooType.softFromHard(Foo(5, "foo", Some(5))))

    val result = new CommandToSqlTransformer(schema).createToInsertSql(command)
    result should be(
      InsertSql(
        table,
        SqlEntity(
          Seq(
            SqlValue(aColumn, 5),
            SqlValue(bColumn, "foo"),
            SqlValue(dColumn, Some(5))
          )
        )

      )
    )
  }


  it should "create query from delete" in {
    val command = Delete(SoftID1(5)(fooType))

    val result = new CommandToSqlTransformer(schema).deleteToDropSql(command)
    result should be(
      DeleteSql(
        table,
        Where(Set(
          ColumnWhereExpression(aColumn, Equals(5))
        ))
      )
    )
  }


  it should "create query from update" in {
    val command = Update(SoftID1(5)(fooType), fooType.softFromHard(Foo(5, "foo", Some(5))))

    val result = new CommandToSqlTransformer(schema).updateToUpdateSql(command)
    result should be(
      UpdateSql(
        table,
        SqlEntity(
          Seq(
            SqlValue(aColumn, 5),
            SqlValue(bColumn, "foo"),
            SqlValue(dColumn, Some(5))
          )
        ),
        Where(Set(
          ColumnWhereExpression(aColumn, Equals(5))
        ))
      )
    )
  }

}
