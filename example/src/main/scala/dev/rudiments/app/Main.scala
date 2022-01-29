package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{RootRouter, ScalaRORouter, ScalaRouter}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val space : Space = new Space()

  val root = new RootRouter(RootRouter.config(ConfigFactory.load()))

  root.routers(
    Create(
      ID("example"),
      new ScalaRouter(
        ScalaTypes.ScalaString,
        Type.build[Body].asInstanceOf[Ref],
        new Memory(All, All)
      )
    )
  )
  root.routers(Create(ID("file"), new ExampleFile().router))
  root.routers(Create(ID("types"), new ScalaRORouter(ScalaTypes.ScalaString, space("types"))))

  root.bind()
}
