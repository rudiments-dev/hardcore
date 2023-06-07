package dev.rudiments.file

import dev.rudiments.utils.SHA3

import scala.collection.mutable

class Tx(val initial: Map[Path, FS]) {
  val changed: mutable.Map[Path, FS] = mutable.Map.empty
  val log: mutable.Map[Path, FileLog] = mutable.Map.empty

  def put(k: Path, v: FS): Unit = {
    initial.get(k) match
      case Some(found) =>
        if (found.about != v.about) {
          changed.put(k, v)
          log.put(k, FileLog(Some(found.about.checksum), Some(v.about.checksum)))

          (found, v) match {
            case (Dir(d1, f1), Dir(d2, f2)) =>
              (d1.toSet -- d2.toSet).foreach { s => deleting(k :+ s) }
              (f1.toSet -- f2.toSet).foreach { s => deleting(k :+ s) }
            case (Dir(d, f), _) =>
              d.foreach { s => deleting(k :+ s) }
              f.foreach { s => deleting(k :+ s) }
            case _ => //do nothing
          }
        } else {
          changed.remove(k)
          log.remove(k)
        }
      case None =>
        v match {
          case NotExist =>
            changed.remove(k)
            log.remove(k)
          case _ =>
            changed.put(k, v)
            log.put(k, FileLog(None, Some(v.about.checksum)))
        }
  }

  def makeCommit: Commit = Commit(changed.toMap.map((k,change) => k -> (log(k), change)))

  def deleting(from: Path): Unit = { //TODO replace with Delete on Dir instead of Repo
    initial.get(from) match {
      case Some(d@Dir(files, dirs)) =>
        changed.put(from, NotExist)
        log.put(from, FileLog(None, Some(d.about.checksum)))
        dirs.foreach(s => deleting(from :+ s))
        files.foreach(s => deleting(from :+ s))
      case Some(b: Binary) =>
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
  changes: Map[Path, (FileLog, FS)]
) {
  def changed: Map[Path, FS] = changes.map { case (k, (_, change)) => k -> change }
}