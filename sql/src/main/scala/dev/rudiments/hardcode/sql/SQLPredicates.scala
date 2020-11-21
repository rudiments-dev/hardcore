package dev.rudiments.hardcode.sql

import dev.rudiments.domain.ADT

sealed trait SQLPredicate extends ADT {
  def sql(column: String, key: String): SqlPart
}

object SQLPredicates {

  case object IsNull extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column IS NULL")
  }
  case object NotNull extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column IS NOT NULL")
  }
  case class Equals(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column = {$key}", Seq(Binding(key, value)))
  }
  case class NotEquals(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column <> {$key}", Seq(Binding(key, value)))
  }
  case class In(in: Seq[Any]) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = {
      val indexed = in.zipWithIndex
      val bindings = indexed.map(i => Binding(key + "_" + i._2.toString, i._1))
      val text = column + " IN " + bindings.map(_.key).mkString("{", ",", "}")
      SqlPart(text, bindings)
    }
  }
  case class Greater(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column > {$key}", Seq(Binding(key, value)))
  }
  case class GreaterOrEquals(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column >= {$key}", Seq(Binding(key, value)))
  }
  case class Less(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column < {$key}", Seq(Binding(key, value)))
  }
  case class LessOrEquals(value: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart = SqlPart(s"$column <= {$key}", Seq(Binding(key, value)))
  }
  case class Between(from: Any, to: Any) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart =
      SqlPart(s"$column BETWEEN {${key}_from} AND {${key}_to}", Seq(
        Binding(key + "_from", from), Binding(key + "_to", to)
      ))
  }

  case class StartsWith(value: String) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart =
      SqlPart(s"$column LIKE '{$key}%'", Seq(Binding(key, value))) // TODO check, should not work because of ''
  }
  case class EndsWith(value: String) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart =
      SqlPart(s"$column LIKE '%{$key}'", Seq(Binding(key, value)))
  }
  case class Contains(value: String) extends SQLPredicate {
    def sql(column: String, key: String): SqlPart =
      SqlPart(s"$column LIKE '%{$key}%'", Seq(Binding(key, value)))
  }
}