package dev.rudiments.hardcore.http.query.interop

import com.sun.tools.javac.code.TypeTag
import dev.rudiments.hardcore.http.query.{EqualsBlueprint, LessBlueprint, MoreBlueprint, QueryBlueprint, StartsWithBlueprint}


object Compiler {


  def compile[T : TypeTag](blueprint: QueryBlueprint[T]) = {
    def getValue(fieldName: String): Any = implicitly[TypeTag[T]].getClass.getDeclaredField(fieldName).get()
    blueprint.parts.map {
      case blueprint: EqualsBlueprint[_] =>
      case LessBlueprint(fieldName, value) =>
      case MoreBlueprint(fieldName, value) =>
      case StartsWithBlueprint(fieldName, value) =>
      case _ =>
    }
  }
}
