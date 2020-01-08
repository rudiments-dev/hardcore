package dev.rudiments.hardcore.http.query

case class HttpParams(query: String) {

  val parts: Seq[Param] = query.split(";").map { str =>
    val splited = str.split("=").head

    val fieldName = if (splited.contains(".")) {
      splited.split("\\.").head
    } else splited
    Param(
      fieldName,
      str
    )
  }.toSeq

}

case class Param(fieldName: String, text: String)

object Param {
  def apply(string: String): Param = {
    val parts = string.split("=")
    new Param(
      parts.head, string
    )
  }
}