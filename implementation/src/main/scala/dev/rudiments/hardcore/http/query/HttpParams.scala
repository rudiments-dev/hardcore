package dev.rudiments.hardcore.http.query

case class HttpParams(query: String) {
  //todo think about ;
  val parts: Seq[Param] = query.split(";").map { str =>
    Param(
      str.split("=").head,
      str
    )
  }.toSeq

}

case class Param(fieldName: String, text: String)