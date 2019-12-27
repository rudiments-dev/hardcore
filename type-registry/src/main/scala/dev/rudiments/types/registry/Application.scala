package dev.rudiments.types.registry

import java.sql.{Date, Time, Timestamp}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, Find, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.{IDPath, RootRouter, Router}
import dev.rudiments.hardcore.types.{CollectionFlags, DTO, Defaults, Field, FieldFlag, FieldFlags, FieldType, ID, Type, TypeSystem}
import enumeratum.{Enum, EnumEntry}
import io.circe.{Encoder, Json}

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  implicit val t: Type[TypeSystem] = Type[TypeSystem]

  try {
    val config = ConfigFactory.load()
    val db = new DataMemoryAdapter[TypeSystem]

    import dev.rudiments.hardcore.http.CirceSupport._
    implicit def typeEncoder: Encoder[Type[_]] = new Encoder[Type[_]] {
      override def apply(a: Type[_]): Json = Json.obj(
        "name" -> Json.fromString(a.name),
        "fields" -> Json.obj(
          a.fields.map { case (fieldName, Field(t, f)) =>
            fieldName -> (f match {
              case FieldFlags.Optional => Json.fromString(t.toString + "?")
              case FieldFlags.Required => Json.fromString(t.toString + "!")
              case FieldFlags.WithDefault => Json.fromString(t.toString + "+")
              case CollectionFlags.CanBeEmpty => Json.fromString(t.toString + "[]")
              case CollectionFlags.NonEmpty => Json.fromString(t.toString + "[!]")
              case CollectionFlags.Nullable => Json.fromString(t.toString + "[]?")
              case CollectionFlags.WithDefault => Json.fromString(t.toString + "[+]")
            })
          }.toSeq: _*
        ),
      )
    }

    db(Create(ID("sample"), TypeSystem("sample", Type[Example], Type[Sample])))

    val port = new ReadOnlyHttpPort[TypeSystem]("types", IDPath[TypeSystem, String], db)
    new RootRouter(config, port).bind()
  } catch {
    case e: Throwable =>
      logger.error("Error while initializing app, shutdown", e)
      actorSystem.terminate().onComplete {
        case Success(t) => logger.info("Terminated {}", t)
        case Failure(err) =>
          logger.error("Termination failed with error", err)
          sys.exit(-1)
      }
  }

  sealed trait MyEnum extends EnumEntry
  object MyEnum extends Enum[MyEnum] {
    override def values: immutable.IndexedSeq[MyEnum] = findValues

    case object One extends MyEnum
    case object Two extends MyEnum
    case object Red extends MyEnum
  }

  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None,
    n: Int = Int.MaxValue,
    array: Seq[Int],
    arrayWithDefault: Seq[Int] = Seq.empty,
    question: List[Int] = List(42),
    when: Timestamp = Defaults.now,
    date: Option[Date] = Some(Defaults.today)
  ) extends DTO

  case class Sample(
    string: String,
    optString: Option[String],
    defaultString: String = "default",
    defaultOptString: Option[String] = None,
    listOfStrings: Seq[String],
    int: Int,
    optInt: Option[Int],
    double: Double,
    optDouble: Option[Double],
    long: Long,
    optLong: Option[Long],
    decimal: BigDecimal,
    optDecimal: Option[BigDecimal],
    timestamp: Timestamp,
    optTimestamp: Option[Timestamp],
    date: Date,
    optDate: Option[Date],
    time: Time,
    optTime: Option[Time],
    enum: MyEnum,
    optEnum: Option[MyEnum]
  ) extends DTO
}
