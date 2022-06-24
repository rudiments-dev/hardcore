package dev.rudiments.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{RootRouter, ScalaRouter}

object Main extends App {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val root: Memory = new Memory()
  private val testRouter = new ScalaRouter().routes
  new RootRouter(RootRouter.config(ConfigFactory.load()), "example" -> testRouter).bind()

  private val t = Type(Field("a", Bool))

  root.execute(Commit(
    Map(
      ID("42") -> Created(Data(t, Seq(true))),
      ID("24") -> Created(Data(t, Seq(false))),
      ID("32") -> Created(Data(t, Seq(false)))
    ), null
  ))
}
