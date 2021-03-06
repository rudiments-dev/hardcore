package dev.rudiments.example

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.Defaults
import dev.rudiments.data._
import dev.rudiments.data.{SoftApp, SoftModule}
import dev.rudiments.hardcore.http.HttpPorts.DependencyLess
import dev.rudiments.hardcore.{Command, Error, Event}
import dev.rudiments.domain._
import io.circe.Encoder

object TodoApp extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()
  private implicit val domain: Domain = new Domain
  implicit val t: Spec = domain.makeFromScala[Spec, Item]

  import akka.http.scaladsl.server.Directives._
  import dev.rudiments.hardcore.http.CirceSupport._

  private val todoItemModule = SoftModule("todo", "id", Seq.empty, Seq(
    "done" -> (ctx => id => DependencyLess.EmptyPostPort[Done, Event](
      Done(id),
      {
        case Done(id) =>
          ctx.adapter(Find(id)).flatMap[Found] { found =>
            found.value.copy[Boolean]("done", {
              case d if !d => Right(!d)
              case d if d => Left(AlreadyDone(found.value))
            }).map(it => ctx.adapter(Update(id, it))).merge
          }
      },
      {
        case Updated(_, _, value) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.OK, value)
        case AlreadyDone(item) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.Conflict, item)
      }
    )),
    "undone" -> (ctx => id => DependencyLess.EmptyPostPort[Undone, Event](
      Undone(id),
      {
        case Done(id) =>
          ctx.adapter(Find(id)).flatMap[Found] { found =>
            found.value.copy[Boolean]("done", {
              case d if d => Right(!d)
              case d if !d => Left(AlreadyNotDone(found.value))
            }).map(it => ctx.adapter(Update(id, it))).merge
          }
      },
      {
        case Updated(_, _, value) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.OK, value)
        case AlreadyNotDone(item) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.Conflict, item)
      }
    )),
  ))

  todoItemModule.context.adapter(Create(ID(Seq(1L)), Instance(t, Seq(1L, "TODO Item #1", false, None))))
  todoItemModule.context.adapter(Create(ID(Seq(2L)), Instance(t, Seq(2L, "TODO Item #2", true, None))))
  todoItemModule.context.adapter(Create(ID(Seq(3L)), Instance(t, Seq(3L, "TODO Item #3", false, Some("comment for #3")))))

  new SoftApp(ConfigFactory.load(), todoItemModule).init()

  private case class Item(
    id: Long = Defaults.long,
    name: String,
    done: Boolean,
    comment: Option[String] = None
  ) extends DTO

  private case class Done(id: ID) extends Command
  private case class AlreadyDone(item: Instance) extends Error

  private case class Undone(id: ID) extends Command
  private case class AlreadyNotDone(item: Instance) extends Error
}