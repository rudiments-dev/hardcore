package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.management.Management
import dev.rudiments.hardcore.file.FileAgent
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}

object Main extends App {
  private implicit val actorSystem: ActorSystem = ActorSystem()

  private val mem: Memory = new Memory()
  mem << Management.typesCommit
  mem << Management.locationsCommit

  val files = ID("files")
  mem += files -> Node.empty
  mem /! files << uploadFiles

  private val testRouter = new ScalaRouter(mem.node).routes
  new RootRouter(
    RootRouter.config(ConfigFactory.load()),
    "example" -> testRouter
  ).bind()

  private def uploadFiles: Commit = {
    val fileAgent = new FileAgent(".")
    fileAgent.reconsFor(mem /! files) match {
      case Prepared(cmt) =>
        cmt
      case _ =>
        throw new IllegalStateException("Unexpected result of load")
    }
  }
}
