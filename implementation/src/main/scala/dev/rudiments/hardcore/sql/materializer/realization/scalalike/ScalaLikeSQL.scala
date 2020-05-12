package dev.rudiments.hardcore.sql.materializer.realization.scalalike

import dev.rudiments.hardcore.sql.SQL
import dev.rudiments.hardcore.sql.materializer.Binding
import dev.rudiments.hardcore.types.SoftInstance


case class ScalaLikeSQL(raw: String, bindings: Set[Binding]) extends SQL[SoftInstance] {

  override def exec(): SoftInstance = ???
//    SQL(raw).bindByName(bindings.map { case Binding(key, value) =>
//      Symbol(key) -> value
//    }.tzzoSeq :_*).execute().apply()
//    SoftInstance.apply()
//  }

}
