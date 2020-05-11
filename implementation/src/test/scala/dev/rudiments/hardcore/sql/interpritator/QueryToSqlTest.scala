package dev.rudiments.hardcore.sql.interpritator

import dev.rudiments.hardcore.data.CRUD.{Create, Delete}
import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.predicates.{IntEquals, StringEquals}
import dev.rudiments.hardcore.sql.{DeleteSql, InsertSql, SelectSql, SqlEntity, SqlValue}
import dev.rudiments.hardcore.sql.parts.{Equals, From, Select, Selector, Where, ColumnWhereExpression}
import dev.rudiments.hardcore.sql.schema.{Column, ColumnTypes, Schema, Table}
import dev.rudiments.hardcore.types.HardID.HardID0
import dev.rudiments.hardcore.types.ID
import dev.rudiments.hardcore.types.{DTO, HardType, ID}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class QueryToSqlTest extends FlatSpec with Matchers {

  case class Foo(a: Int, b: String, d: Option[Int]) extends DTO

  implicit val tt: HardType[Foo] = HardType.apply[Foo].copy(primaryKeys = Seq("a"))

  val aColumn: Column = Column("a", ColumnTypes.INT, nullable = false, default = false, pk = false)
  val bColumn: Column = Column("b", ColumnTypes.VARCHAR(150), nullable = false, default = false, pk = false)
  val dColumn: Column = Column("d", ColumnTypes.INT, nullable = true, default = false, pk = false)

  val table: From = Table(
    "Foo",
    Seq(
      aColumn,
      bColumn,
      dColumn
    ),
    Seq.empty
  )

  val schema: Schema = Schema(
    "dev",
    Map(
      tt -> table
    ),
    Set.empty
  )

  it should "test select query generation" in {
    val query = Query[Foo](Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ))

    val result = QueryToSql(schema).queryToSelectSql(query, tt)
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
    val command = Create(HardID0[Foo](), Foo(5, "foo", Some(5)))

    val result = QueryToSql(schema).createToInsertSql(command)
    result should be(
      InsertSql(
        table,
        Seq(
          SqlEntity(
            Seq(
              SqlValue(aColumn, 5),
              SqlValue(bColumn, "foo"),
              SqlValue(dColumn, Some(5))
            )
          )
        )
      )
    )
  }


  it should "create query from delete" in {
    val command = Delete(HardID0[Foo](), Foo(5, "foo", Some(5)))

    val result = QueryToSql(schema).createToInsertSql(command)
    result should be(
      DeleteSql(
        table,
        Seq(
          SqlEntity(
            Seq(
              SqlValue(aColumn, 5),
              SqlValue(bColumn, "foo"),
              SqlValue(dColumn, Some(5))
            )
          )
        )
      )
    )
  }


  it should "test query from create" in {
    val command = Create(HardID0[Foo](), Foo(5, "foo", Some(5)))

    val result = QueryToSql(schema).createToInsertSql(command)
    result should be(
      InsertSql(
        table,
        Seq(
          SqlEntity(
            Seq(
              SqlValue(aColumn, 5),
              SqlValue(bColumn, "foo"),
              SqlValue(dColumn, Some(5))
            )
          )
        )
      )
    )
  }
}
