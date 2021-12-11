package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore.http.RootRouter

object Main extends App {
  val config = ConfigFactory.load()

  implicit val actorSystem: ActorSystem = ActorSystem()

  new RootRouter(config, new Example().router).bind()
}
