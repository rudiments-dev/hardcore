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
  space -> Create(ID("project-files"), Dir("."))
  space -> Create(ID("example"), new Memory(All, All))

  Type.build[Body]

  ThingEncoder.init
  FileEncoder.init

  val root = new RootRouter(RootRouter.config(ConfigFactory.load()))
  space -> Create(ID("router-memory"), root.routers)
  val rm = Path("router-memory")

  rm -> Create(ID("example"), new ScalaRouter(ScalaString, Path("types/Body").ref, Path("example")))
  rm -> Create(ID("file"), new ScalaRORouter(ScalaString, Path("project-files")))
  rm -> Create(ID("types"), new ScalaRORouter(ScalaString, Path("types")))
  rm -> Create(ID("routers"), new ScalaRORouter(ScalaString, rm))
  rm -> Create(ID("all"), new ScalaRORouter(ScalaString, / ))

  root.bind()
}
