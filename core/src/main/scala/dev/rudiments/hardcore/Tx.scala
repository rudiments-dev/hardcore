package dev.rudiments.hardcore

import java.lang
import scala.collection.mutable

case class Tx(
  root: Node,
  log: mutable.LinkedHashMap[Location, mutable.Seq[Out with CRUD]]
) {

}
