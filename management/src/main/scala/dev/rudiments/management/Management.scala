package dev.rudiments.management

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.Initial.types

object Management {
  val userType = Type(
    Field("name", Text(1024)),
    Field("email", Text(1024))
  )
  val userLink = Link(types / "User", userType)
  val taskType = Type(
    Field("name", Text(4096)),
    Field("summary", Text(4 * 1024 * 1024)),
    Field("deadline", Date)
//    Field("assigned", userLink)
  )
  private val taskLink = Link(types / "Task", taskType)

  val typesCommit: Commit = Commit(Map(
    types / "User" -> Created(userType),
    types / "Task" -> Created(taskType)
  ))

  val work: Location = ID("work")
  val docs: Location = work / "docs"
  val team: Location = work / "team"
  val tasks: Location = work / "tasks"
  val meetings: Location = work / "meetings"

  val locationsCommit: Commit = Commit(Map(
    work -> Created(Node.empty),
    docs -> Created(Node.empty),
    team -> Created(Node(leafIs = userLink)),
    tasks -> Created(Node(leafIs = taskLink)),
    meetings -> Created(Node.empty)
  ))


}
