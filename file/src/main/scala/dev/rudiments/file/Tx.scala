package dev.rudiments.file

import dev.rudiments.utils.SHA3

import scala.collection.mutable

class Tx(val initial: Map[Rel, FileData]) {
  val changed: mutable.Map[Rel, FileData] = mutable.Map.empty
  val log: mutable.Map[Rel, FileLog] = mutable.Map.empty

  def put(k: Rel, v: FileData): Unit = {
    initial.get(k) match
      case Some(found) =>
        if (found.about != v.about) { //TODO Dir change => delete files
          changed.put(k, v)
          log.put(k, FileLog(Some(found.about.checksum), Some(v.about.checksum)))
        } else {
          changed.remove(k)
          log.remove(k)
        }
      case None =>
        changed.put(k, v)
        log.put(k, FileLog(None, Some(v.about.checksum)))
  }

  def makeCommit: Commit = Commit(changed.toMap.map((k,change) => k -> (log(k), change)))

  def deleting(from: Rel): Unit = {
    initial.get(from) match {
      case Some(d@Dir(files, dirs)) =>
        changed.put(from, NotExist)
        log.put(from, FileLog(None, Some(d.about.checksum)))
        dirs.foreach(s => deleting(from :+ s))
        files.foreach(s => deleting(from :+ s))
      case Some(b: Blob) =>
        changed.put(from, NotExist)
        log.put(from, FileLog(None, Some(b.about.checksum)))
      case _ => //DO nothing
    }
  }
}


case class FileLog(
  before: Option[SHA3],
  after: Option[SHA3]
)

case class Commit(
  changes: Map[Rel, (FileLog, FileData)]
) {
  def changed: Map[Rel, FileData] = changes.map { case (k, (_, change)) => k -> change }
}