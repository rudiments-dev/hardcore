package dev.rudiments.types.registry

import java.sql.{Date, Time, Timestamp}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.{Create, DataMemoryAdapter, ReadOnlyHttpPort}
import dev.rudiments.hardcore.http.{IDPath, RootRouter}
import dev.rudiments.hardcore.types.{FieldType, _}
import enumeratum.{Enum, EnumEntry}
import io.circe.{Encoder, Json}

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Application extends App with LazyLogging {
  logger.info("Starting application")

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  implicit val t: HardType[TypeSystem] = HardType[TypeSystem]

  try {
    val config = ConfigFactory.load()
    val db = new DataMemoryAdapter[TypeSystem]

    import dev.rudiments.hardcore.http.CirceSupport._
    implicit def typeEncoder: Encoder[HardType[_]] = new Encoder[HardType[_]] {

      private def fieldFormat(t: FieldType): String = t match {
        case RudimentTypes.Text =>      RudimentTypes.Text.toString
        case RudimentTypes.Number =>    RudimentTypes.Number.toString

        case RudimentTypes.Date =>      RudimentTypes.Date.toString
        case RudimentTypes.Time =>      RudimentTypes.Time.toString
        case RudimentTypes.Timestamp => RudimentTypes.Timestamp.toString

        case RudimentTypes.Enum(n, v) => n.split("\\.").last + v.mkString("[", ",", "]")

        case RudimentTypes.List(f) => fieldFormat(f) // CollectionFlag will do the thing
        case RudimentTypes.Set(f) =>  fieldFormat(f) // CollectionFlag will do the thing
        case RudimentTypes.Index(k, v) => fieldFormat(k) + "->" + fieldFormat(v)

        case RudimentTypes.Reference(another) => "*" + another.name

        case RudimentTypes.Unknown => "UNKNOWN"
        case _ => throw new IllegalArgumentException
      }

      override def apply(a: HardType[_]): Json = Json.obj(
        "name" -> Json.fromString(a.name),
        "fields" -> Json.obj(
          a.fields.map { case (fieldName, Field(t, f)) =>
            val formated = fieldFormat(t)
            fieldName -> (f match {
              case FieldFlags.Optional => Json.fromString(formated + "?")
              case FieldFlags.Required => Json.fromString(formated + "!")
              case FieldFlags.WithDefault => Json.fromString(formated + "+")
              case CollectionFlags.CanBeEmpty => Json.fromString(formated + "[]")
              case CollectionFlags.NonEmpty => Json.fromString(formated + "[!]")
              case CollectionFlags.Nullable => Json.fromString(formated + "[]?")
              case CollectionFlags.WithDefault => Json.fromString(formated + "[+]")
            })
          }.toSeq: _*
        ),
      )
    }

    db(Create(ID("sample"), TypeSystem("my-type-system", HardType[Example], HardType[Sample])))

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
    date: Option[Date] = Some(Defaults.today),
    mapIntDate: Map[Int, Date] = Map.empty,
    sample: Sample,
    optSample: Option[Sample],
    seqSample: Seq[Sample] = Seq.empty,
    setSample: Set[Sample] = Set.empty,
    mapSample: Map[String, Sample] = Map.empty
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
