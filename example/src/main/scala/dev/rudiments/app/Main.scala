package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore.ScalaTypes.ScalaString
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.Path._
import dev.rudiments.hardcore.file.Dir
import dev.rudiments.hardcore.http.{RootRouter, ScalaRORouter, ScalaRouter, ThingEncoder}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val space : Space = new Space()
  init(space)
  Path("router").find[RootRouter].bind()


  def init(implicit space: Space): Unit = {
    ThingEncoder.init
    FileEncoder.init
    space << Create(ID("project-files"), Dir("."))
    space << Create(ID("example"), new Memory(ScalaString, Type.build[Body]))

    space << Create(ID("router"), new RootRouter(RootRouter.config(ConfigFactory.load())))

    val router = Path("router")
    router << Create(ID("example"), new ScalaRouter(ScalaString, Path("types/Body").ref, Path("example")))
    router << Create(ID("file"), new ScalaRORouter(ScalaString, Path("project-files")))
    router << Create(ID("types"), new ScalaRORouter(ScalaString, Path("types")))
    router << Create(ID("routers"), new ScalaRORouter(ScalaString, router))
    router << Create(ID("all"), new ScalaRORouter(ScalaString, / ))
  }
}
