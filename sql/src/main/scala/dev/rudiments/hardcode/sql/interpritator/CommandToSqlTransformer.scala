package dev.rudiments.hardcode.sql.interpritator

import dev.rudiments.hardcore.data.soft.ReadOnly.Find
import dev.rudiments.hardcore.data.soft.SoftCRUD.{Create, Delete, Update}
import dev.rudiments.hardcore.http.query.HttpQuery
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.hardcode.sql.schema.{Column, Schema, Table}
import dev.rudiments.hardcore.types.SoftID.{SoftID0, SoftID1, SoftID2}
import dev.rudiments.hardcore.types.{ID, SoftID, Type}
import dev.rudiments.hardcode.sql.SQLDataClasses._
import dev.rudiments.hardcode.sql.SQLPredicates._
import dev.rudiments.hardcode.sql.SQLParts._
import dev.rudiments.hardcode.sql.SQLPredicate

class CommandToSqlTransformer(schema: Schema) {

  def queryToSelectSql(query: HttpQuery): QueryDataClass = {
    val table = schema.tables(query.softType)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap


    val selectors = query.softType.fields.keys.map { field =>
      Selector(
        fieldToColumn(field), None
      )
    }.toSeq

    val converterFunction = partToWhereExpression(table)

    QueryDataClass(
      Select(selectors),
      From(schema, table, None),
      Where(query.parts.map(converterFunction)),
      query.softType
    )
  }

  def findToFindByIdSql(find: Find): FindByIdDataClass = {
    val t = find.key.asInstanceOf[SoftID].t
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val selectors = t.fields.keys.map { field =>
      Selector(
        fieldToColumn(field), None
      )
    }.toSeq

    FindByIdDataClass(
      Select(selectors),
      From(schema, table, None),
      idToWhere(table, t)(find.key),
      t,
      find.key
    )
  }

  def createToInsertSql(create: Create): InsertDataClass = {
    val t = create.key.asInstanceOf[SoftID].t
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(t.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), t.extract(create.value, field))
    }.toSeq)

    val findByIdDataClass = FindByIdDataClass(
      Select(t.fields.keys.map { field =>
        Selector(
          fieldToColumn(field), None
        )
      }.toSeq),
      From(schema, table, None),
      idToWhere(table, t)(create.key),
      t,
      create.key
    )

    InsertDataClass(
      schema,
      table,
      entity,
      findByIdDataClass,
      t,
      create.value
    )
  }

  def deleteToDropSql(command: Delete): DeleteDataClass = {
    val t = command.key.asInstanceOf[SoftID].t
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val findByIdDataClass = FindByIdDataClass(
      Select(t.fields.keys.map { field =>
        Selector(fieldToColumn(field), None)
      }.toSeq),
      From(schema, table, None),
      idToWhere(table, t)(command.key),
      t,
      command.key
    )

    DeleteDataClass(
      schema,
      table,
      idToWhere(table, t)(command.key),
      t,
      findByIdDataClass
    )
  }

  def updateToUpdateSql(command: Update): UpdateDataClass = {
    val t = command.key.asInstanceOf[SoftID].t
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(t.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), t.extract(command.value, field))
    }.toSeq)

    val findByIdDataClass = FindByIdDataClass(
      Select(t.fields.keys.map { field =>
        Selector(fieldToColumn(field), None)
      }.toSeq),
      From(schema, table, None),
      idToWhere(table, t)(command.key),
      t,
      command.key
    )

    UpdateDataClass(
      schema,
      table,
      entity,
      idToWhere(table, t)(command.key),
      t,
      findByIdDataClass,
      command.value
    )
  }

  private def partToWhereExpression(table: Table): PartialFunction[Predicate[_], ColumnWhereExpression] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    def predicateFunc: PartialFunction[Predicate[_], SQLPredicate] = {
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

  private def idToWhere(table: Table, t: Type): PartialFunction[ID, Where] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap
    val function: PartialFunction[ID, Set[WhereExpression]] = {
      case _: SoftID0 => throw new UnsupportedOperationException("SoftID0 not supported for where expression")
      case id: SoftID1 =>
        val keys = t.primaryKeys
        Set(
          ColumnWhereExpression(
            fieldToColumn(keys.head),
            Equals(id.key)
          )
        )
      case id: SoftID2 =>
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
      case other => throw new UnsupportedOperationException(s"Not supported generation for : ${other}")
    }

    {
      case id => Where(function(id))
    }
  }
}
