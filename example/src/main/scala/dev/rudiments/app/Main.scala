package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.FileAgent
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}

object Main extends App {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val ctx: Memory = new Memory()
  private val files = ID("files")
  private val fileAgent = new FileAgent("./core/src")
  ctx += files -> Node.empty
  fileAgent.reconsFor(ctx /! files) match {
    case Prepared(cmt) =>
      ctx /! files << cmt
    case _ =>
      throw new IllegalStateException("Unexpected result of load")
  }
  private val testRouter = new ScalaRouter(ctx.node).routes
  new RootRouter(
    RootRouter.config(ConfigFactory.load()),
    "example" -> testRouter
  ).bind()
}
