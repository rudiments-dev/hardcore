package dev.rudiments.management

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.Initial.types

object Management {
  val work: Location = ID("work")
  val team: Location = work / "team"
  val tasks: Location = work / "tasks"
  val boards: Location = work / "boards"
  val docs: Location = work / "docs"
  val meetings: Location = work / "meetings"

  def init(ctx: Node): Commit = {
    val tx = new Tx(ctx)

    tx += types / "User" -> Type(
      Field("name", Text(1024)),
      Field("email", Text(1024))
    )
    tx += types / "TaskStatus" -> Node.partnership(types, Seq("TODO", "InProgress", "Done"))
    tx += types / "TODO" -> Nothing
    tx += types / "InProgress" -> Nothing
    tx += types / "Done" -> Nothing
    tx += types / "Task" -> Type(
      Field("name", Text(4096)),
      Field("summary", Text(4 * 1024 * 1024)),
      Field("deadline", Date),
      Field("status", tx ! (types / "TaskStatus"))
    )
    tx += types / "BoardColumn" -> Type(
      Field("tasks", Enlist(tx ! (types / "Task")))
    )

    tx += work -> Node.empty
    tx += team -> Node(leafIs = tx ! (types / "User"))
    tx += tasks -> Node(leafIs = tx ! (types / "Task"))
    tx += boards -> Node(leafIs = Nothing)//TODO add node constraint - only `node = Node(leafIs = #BoardColumn)` allowed
    tx += docs -> Node.empty
    tx += meetings -> Node.empty

    val prepared = tx.>>

    prepared match {
      case Prepared(c) => ctx << c match {
        case Committed(cmt) =>
          cmt
        case _ => throw new IllegalStateException("Management commit failed")
      }
      case _ => throw new IllegalStateException("Management commit not prepared")
    }
  }


}
