package dev.rudiments.file

import dev.rudiments.utils.SHA3

import scala.collection.mutable

class Tx(val initial: Map[Seq[String], FileData]) {
  val changed: mutable.Map[Seq[String], FileData] = mutable.Map.empty
  val log: mutable.Map[Seq[String], FileLog] = mutable.Map.empty

  def put(k: Seq[String], v: FileData): Unit = {
    initial.get(k) match //TODO drop when was in changed but not anymore
      case Some(found) =>
        if (found.about != v.about) {
          changed.put(k, v)
          log.put(k, FileLog(Some(found.about.checksum), Some(v.about.checksum)))
        }
      case None =>
        changed.put(k, v)
        log.put(k, FileLog(None, Some(v.about.checksum)))
  }

  def makeCommit: Commit = Commit(changed.toMap.map((k,change) => k -> (log(k), change)))
}


case class FileLog(
  before: Option[SHA3],
  after: Option[SHA3]
)

case class Commit(
  changes: Map[Seq[String], (FileLog, FileData)]
) {
  def changed: Map[Seq[String], FileData] = changes.map { case (k, (_, change)) => k -> change }
}