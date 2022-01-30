package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.file.Dir
import dev.rudiments.hardcore.http.{RootRouter, ScalaRORouter, ScalaRouter, ThingEncoder}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val space : Space = new Space()
  ThingEncoder.init
  FileEncoder.init

  val root = new RootRouter(RootRouter.config(ConfigFactory.load()))

  root.routers(Create(ID("example"),
    new ScalaRouter(
      ScalaTypes.ScalaString,
      Type.build[Body].asInstanceOf[Ref],
      new Memory(All, All)
    )
  ))
  root.routers(Create(ID("file"), new ScalaRORouter(ScalaTypes.ScalaString, Dir("."))))
  root.routers(Create(ID("types"), new ScalaRORouter(ScalaTypes.ScalaString, space("types"))))
  root.routers(Create(ID("routers"), new ScalaRORouter(ScalaTypes.ScalaString, root.routers)))

  root.bind()
}
