package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.FileAgent
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}

object Main extends App {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val ctx: Context = new Context()
  private val fileAgent = new FileAgent(".", Root)
  fileAgent.load(Root, ctx) match {
    case Prepared(cmt) => ctx << cmt
    case _ => throw new IllegalStateException("Unexpected result of load")
  }
  private val testRouter = new ScalaRouter(ctx).routes
  new RootRouter(
    RootRouter.config(ConfigFactory.load()),
    "example" -> testRouter
  ).bind()
}
