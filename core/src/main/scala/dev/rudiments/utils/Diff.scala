package dev.rudiments.utils

import scala.jdk.CollectionConverters.*
import com.github.difflib.{DiffUtils, UnifiedDiffUtils, patch}
import com.github.difflib.patch.{AbstractDelta, ChangeDelta, DeleteDelta, DeltaType, EqualDelta, InsertDelta, Patch}

case class Diff[T](
  deltas: Seq[Delta[T]]
) {
  def applyTo(to: Seq[T]): Seq[T] = {
    this.asJava.applyTo(to.asJava).asScala.toSeq
  }

  def asJava: patch.Patch[T] = {
    val p = new Patch[T](deltas.size)
    deltas.reverse.foreach { d => p.addDelta(d.asJava) }
    p
  }
}

object Diff {
  def apply[T](from: Seq[T], to: Seq[T]): Diff[T] = {
    fromPatch[T](DiffUtils.diff(from.asJava, to.asJava))
  }

  def fromPatch[T](patch: Patch[T]): Diff[T] = {
    Diff(
      patch.getDeltas.asScala.toSeq.map {
        case d: AbstractDelta[T] if d.getType == DeltaType.INSERT =>
          Delta.Insert(Chunk.apply[T](d.getTarget))
        case d: AbstractDelta[T] if d.getType == DeltaType.CHANGE =>
          Delta.Change(Chunk.apply[T](d.getSource), Chunk.apply[T](d.getTarget))
        case d: AbstractDelta[T] if d.getType == DeltaType.DELETE =>
          Delta.Delete(Chunk.apply[T](d.getSource))
        case d: AbstractDelta[T] if d.getType == DeltaType.EQUAL =>
          Delta.Equals(Chunk.apply[T](d.getSource))
      }
    )
  }

  def fromUnified(diff: Seq[String]): Diff[String] = {
    Diff.fromPatch(
      UnifiedDiffUtils.parseUnifiedDiff(diff.asJava)
    )
  }
}

object Unified {
  def generate(
    from: Seq[String], fromName: String,
    to: Seq[String], toName: String,
    contextSize: Int
  ): Seq[String] = {
    UnifiedDiffUtils.generateUnifiedDiff(
      fromName, toName, from.asJava, Diff(from, to).asJava, contextSize
    ).asScala.toSeq
  }
}

enum Delta[T]:
  def asJava: patch.AbstractDelta[T] = this match {
    case Delta.Insert(target) => new InsertDelta[T](target.asJava, target.asJava)
    case Delta.Change(source, target) => new ChangeDelta[T](source.asJava, target.asJava)
    case Delta.Delete(source) => new DeleteDelta[T](source.asJava, new patch.Chunk[T](source.position, Seq.empty.asJava))
    case Delta.Equals(source) => new EqualDelta[T](source.asJava, source.asJava)
  }
  case Insert(target: Chunk[T])
  case Change(source: Chunk[T], target: Chunk[T])
  case Delete(source: Chunk[T])
  case Equals(source: Chunk[T])


case class Chunk[T](
  position: Int,
  lines: Seq[T],
  changes: Seq[Int] = Seq.empty
) {
  def asJava: patch.Chunk[T] = {
    if(changes.isEmpty) {
      new patch.Chunk[T](position, lines.asJava)
    } else {
      new patch.Chunk[T](position, lines.asJava, changes.map(scala.Int.box).asJava)
    }
  }
}

object Chunk {
  def apply[T](c: patch.Chunk[T]): Chunk[T] = {
    new Chunk[T](
      c.getPosition,
      c.getLines.asScala.toSeq,
      if(c.getChangePosition == null) Seq.empty else c.getChangePosition.asScala.map(_.toInt).toSeq
    )
  }
}