package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.FileAgent
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}

object Main extends App {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val mem: Memory = new Memory()
  private val fileAgent = new FileAgent(".")
  fileAgent.load(Root, mem) match {
    case Prepared(cmt) => mem << cmt
    case _ => throw new IllegalStateException("Unexpected result of load")
  }
  private val testRouter = new ScalaRouter(mem).routes
  new RootRouter(
    RootRouter.config(ConfigFactory.load()),
    "example" -> testRouter
  ).bind()
}
