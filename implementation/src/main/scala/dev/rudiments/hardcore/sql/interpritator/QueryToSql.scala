package dev.rudiments.hardcore.sql.interpritator

import dev.rudiments.hardcore.data.CRUD.{Create, Delete, Update}
import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.hardcore.sql.parts._
import dev.rudiments.hardcore.sql.schema.{Column, Schema, Table}
import dev.rudiments.hardcore.sql.{DeleteSql, InsertSql, SelectSql, SqlEntity, SqlValue, UpdateSql}
import dev.rudiments.hardcore.types.HardID.{HardID0, HardID1, HardID2, HardID3}
import dev.rudiments.hardcore.types.{DTO, HardType, ID, Type}

case class QueryToSql(schema: Schema) {

  //todo mappings
  def queryToSelectSql[T <: DTO](query: Query[T], tt: Type): SelectSql = {
    val table = schema.tables(tt)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap


    val selectors = tt.fields.keys.map { field =>
      Selector(
        fieldToColumn(field), None
      )
    }.toSeq

    val converterFunction = partToWhereExpression(table)

    SelectSql(
      Select(selectors),
      From(table, None),
      Where(query.parts.map(converterFunction))
    )
  }

  def createToInsertSql[T <: DTO : HardType](create: Create[T]): InsertSql = {
    val tt = implicitly[HardType[T]]
    val table = schema.tables(tt)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(tt.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), tt.extract(create.value, field))
    }.toSeq)

    InsertSql(
      table,
      entity
    )
  }

  def deleteToDropSql[T <: DTO : HardType](command: Delete[T]): DeleteSql = {
    val tt = implicitly[HardType[T]]
    val table = schema.tables(tt)

    //todo current by id
    DeleteSql(
      table,
      idToWhere(table, tt)(command.key)
    )
  }

  def updateToUpdateSql[T <: DTO : HardType](command: Update[T]): UpdateSql = {
    val tt = implicitly[HardType[T]]
    val table = schema.tables(tt)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(tt.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), tt.extract(command.value, field))
    }.toSeq)

    UpdateSql(
      table,
      entity,
      idToWhere(table, tt)(command.key)
    )
  }



  private def partToWhereExpression(table: Table): PartialFunction[Predicate[_], ColumnWhereExpression] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    def predicateFunc: PartialFunction[Predicate[_], SqlPredicate] = {
      case IntEquals(_, value) => Equals(value)
      case IntLess(_, value) => Lesser(value)
      case IntMore(_, value) => Greater(value)
      case StringEquals(_, value) => Equals(value)
      case IsEmpty(_) => IsNull
      case IsDefined(_) => NotNull
      case OptionValuePredicate(_, underlying) => predicateFunc(underlying)
    }

    val columnFunc: PartialFunction[Predicate[_], Column] = {
      case field: FieldPredicate[_] => fieldToColumn(field.fieldName)
    }

    {
      case part => ColumnWhereExpression(
        columnFunc(part),
        predicateFunc(part)
      )
    }
  }

  private def idToWhere(table: Table, t: HardType[_]): PartialFunction[ID, Where] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap
    val function: PartialFunction[ID, Set[WhereExpression]] = {
      case _: HardID0[_] => throw new UnsupportedOperationException("SoftID0 not supported for where expression")
      case id: HardID1[_, _] =>
        val keys = t.primaryKeys
        Set(
          ColumnWhereExpression(
            fieldToColumn(keys.head),
            Equals(id.key)
          )
        )
      case id: HardID2[_, _, _] =>
        val keys = t.primaryKeys
        Set(
          ColumnWhereExpression(
            fieldToColumn(keys.head),
            Equals(id.key1)
          ),
          ColumnWhereExpression(
            fieldToColumn(keys(1)),
            Equals(id.key2)
          )
        )
      case id: HardID3[_, _, _, _] =>
        val keys = t.primaryKeys
        Set(
          ColumnWhereExpression(
            fieldToColumn(keys.head),
            Equals(id.key1)
          ),
          ColumnWhereExpression(
            fieldToColumn(keys(1)),
            Equals(id.key2)
          ),
          ColumnWhereExpression(
            fieldToColumn(keys(2)),
            Equals(id.key3)
          )
        )
      case other => throw new UnsupportedOperationException(s"Not supported generation for : ${other}")
    }

    {
      case id => Where(function(id))
    }
  }
}
