package dev.rudiments.hardcore.sql

import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly.FindAll
import dev.rudiments.hardcore.http.query.PredicatesQuery
import dev.rudiments.hardcore.http.query.predicates.{IntEquals, StringEquals}
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes, Table, TypedSchema}
import dev.rudiments.hardcore.types.SoftID.SoftID1
import dev.rudiments.hardcore.types.{DTO, Field, FieldFlag, Infinity, NegativeInfinity, NumberFormat, PositiveInfinity, SoftID, Type, Types}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import dev.rudiments.hardcode.sql.SQLDataClasses._
import dev.rudiments.hardcode.sql.interpritator.CommandToSqlTransformer
import dev.rudiments.hardcode.sql.SQLParts._
import dev.rudiments.hardcode.sql.SQLPredicates._
import dev.rudiments.hardcore.types.ScalaTypes.{ScalaInt, ScalaString}

@RunWith(classOf[JUnitRunner])
class CommandToSqlTransformerTest extends FlatSpec with Matchers {

  case class Foo(a: Int, b: String, d: Option[Int]) extends DTO

  val fooType: Type = Type("Foo", Map(
    "a" -> Field(ScalaInt, FieldFlag.Required),
    "b" -> Field(ScalaString, FieldFlag.Required),
    "d" -> Field(ScalaInt, FieldFlag.Optional),
  ))


  val aColumn: Column = Column("a", ColumnTypes.INT, nullable = false, default = false, pk = true)
  val bColumn: Column = Column("b", ColumnTypes.VARCHAR(150), nullable = false, default = false, pk = false)
  val dColumn: Column = Column("d", ColumnTypes.INT, nullable = true, default = false, pk = false)

  val table: Table = Table(
    "Foo",
    Seq(
      aColumn,
      bColumn,
      dColumn
    )
  )

  val schema: TypedSchema = TypedSchema(
    "dev",
    Map(
      fooType -> table
    ),
    Set.empty
  )

  it should "test select query generation" in {
    val query = PredicatesQuery(Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ), fooType)
    val command = FindAll(query)

    val result = new CommandToSqlTransformer(schema).queryToSelectSql(command)
    result should be(
      QueryDataClass(
        Select(Seq(
          Selector(aColumn, None),
          Selector(bColumn, None),
          Selector(dColumn, None)
        )),
        From(schema, table, None),
        Some(Where(
          Set(
            ColumnWhereExpression(bColumn, Equals("bay")),
            ColumnWhereExpression(aColumn, Equals(5))
          )
        )),
        fooType
      )
    )
  }


  it should "test query from create" in {
    val instance = fooType.fromScala(Foo(5, "foo", Some(5)))
    val id = SoftID1(5)(fooType)
    val command = Create(id, instance)

    val result = new CommandToSqlTransformer(schema).createToInsertSql(command)
    result should be(
      InsertDataClass(
        schema,
        table,
        SqlEntity(
          Seq(
            SqlValue(aColumn, 5),
            SqlValue(bColumn, "foo"),
            SqlValue(dColumn, Some(5))
          )
        ),
        FindByIdDataClass(
          Select(Seq(
            Selector(aColumn, None),
            Selector(bColumn, None),
            Selector(dColumn, None)
          )),
          From(schema, table, None),
          Where(
            Set(
              ColumnWhereExpression(aColumn, Equals(5))
            )
          ),
          fooType,
          id
        ),
        fooType,
        instance
      )
    )
  }


  it should "create query from delete" in {
    val id = SoftID1(5)(fooType)
    val command = Delete(id)

    val result = new CommandToSqlTransformer(schema).deleteToDropSql(command)
    result should be(
      DeleteDataClass(
        schema,
        table,
        Where(Set(
          ColumnWhereExpression(aColumn, Equals(5))
        )),
        fooType,
        FindByIdDataClass(
          Select(Seq(
            Selector(aColumn, None),
            Selector(bColumn, None),
            Selector(dColumn, None)
          )),
          From(schema, table, None),
          Where(
            Set(
              ColumnWhereExpression(aColumn, Equals(5))
            )
          ),
          fooType,
          id
        )
      )
    )
  }


  it should "create query from update" in {
    val instance = fooType.fromScala(Foo(5, "foo", Some(5)))
    val id = SoftID1(5)(fooType)
    val command = Update(id, instance)

    val result = new CommandToSqlTransformer(schema).updateToUpdateSql(command)
    result should be(
      UpdateDataClass(
        schema,
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
        )),
        fooType,
        FindByIdDataClass(
          Select(Seq(
            Selector(aColumn, None),
            Selector(bColumn, None),
            Selector(dColumn, None)
          )),
          From(schema, table, None),
          Where(
            Set(
              ColumnWhereExpression(aColumn, Equals(5))
            )
          ),
          fooType,
          id
        ),
        instance
      )
    )
  }

}
