package dev.rudiments.example

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.Defaults
import dev.rudiments.data.CRUD.{Create, Update, Updated}
import dev.rudiments.data.ReadOnly.{Find, Found}
import dev.rudiments.data.{SoftApp, SoftModule}
import dev.rudiments.hardcore.http.HttpPorts.DependencyLess
import dev.rudiments.hardcore.{Command, Error, Event, Failure, Success}
import dev.rudiments.types._
import io.circe.Encoder

object TodoApp extends App with LazyLogging {
  logger.info("Starting application")

  val config = ConfigFactory.load()
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  implicit val t: Type = typeSystem.asType[Item]

  import akka.http.scaladsl.server.Directives._
  import dev.rudiments.hardcore.http.CirceSupport._
  private val todoItemModule = SoftModule("todo", "id", Seq.empty, Seq(
    "done" -> (ctx => id => DependencyLess.EmptyPostPort[Done, Event](
      Done(id),
      {
        case Done(id) =>
          for {
            found <- ctx.adapter(Find(id)).expecting[Found]
            updated <- found.value.copy[Boolean]("done", {
              case d if !d => Right(!d)
              case d if d => Left(AlreadyDone(found.value))
            }).map { instance => ctx.adapter(Update(id, instance)).expecting[Updated] } match {
              case Left(value) => Failure(value)
              case Right(value) => value
            }
          } yield updated
      },
      {
        case Success(Updated(_, _, value)) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.OK, value)
        case Failure(AlreadyDone(item)) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.Conflict, item)
      }
    )),
    "undone" -> (ctx => id => DependencyLess.EmptyPostPort[Undone, Event](
      Undone(id),
      {
        case Undone(id) =>
          for {
            found <- ctx.adapter(Find(id)).expecting[Found]
            updated <- found.value.copy[Boolean]("done", {
              case d if d => Right(!d)
              case d if !d => Left(AlreadyNotDone(found.value))
            }).map { instance => ctx.adapter(Update(id, instance)).expecting[Updated] } match {
              case Left(value) => Failure(value)
              case Right(value) => value
            }
          } yield updated
      },
      {
        case Success(Updated(_, _, value)) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.OK, value)
        case Failure(AlreadyNotDone(item)) =>
          implicit val en: Encoder[Instance] = ctx.encoder
          complete(StatusCodes.Conflict, item)
      }
    )),
  ))

  todoItemModule.context.adapter(Create(ID(1L), Instance(1L, "TODO Item #1", false, None)))
  todoItemModule.context.adapter(Create(ID(2L), Instance(2L, "TODO Item #2", true, None)))
  todoItemModule.context.adapter(Create(ID(3L), Instance(3L, "TODO Item #3", false, Some("comment for #3"))))

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