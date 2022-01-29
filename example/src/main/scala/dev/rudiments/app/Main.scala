package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{RootRouter, ScalaRORouter}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val space : Space = new Space()

  val root = new RootRouter(RootRouter.config(ConfigFactory.load()))
  root.cache(Create(ID("example"), new Example().router))
  root.cache(Create(ID("file"), new ExampleFile().router))
  root.cache(Create(ID("types"), new ScalaRORouter(ScalaTypes.ScalaString, space("types"))))

  root.bind()
}
