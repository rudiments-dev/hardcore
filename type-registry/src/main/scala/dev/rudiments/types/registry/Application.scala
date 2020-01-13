package dev.rudiments.types.registry

import java.sql.{Date, Time, Timestamp}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.data.CRUD.Create
import dev.rudiments.hardcore.data.{DataHttpPort, DataMemoryAdapter}
import dev.rudiments.hardcore.http.RootRouter
import dev.rudiments.hardcore.types._
import enumeratum.{Enum, EnumEntry}

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
    db(Create(ID("sample"), TypeSystem("my-type-system", HardType[Example], HardType[Sample])))

    import dev.rudiments.hardcore.http.CirceSupport._
    import dev.rudiments.types.registry.module.FieldFormat._
    val port = new DataHttpPort[TypeSystem, String]("types", ts => ID[TypeSystem, String](ts.name), db)
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
    mapSample: Map[String, Sample] = Map.empty,
    multimapSample: Map[String, Set[Sample]] = Map.empty,
    deepMapSample: Map[String, Map[String, Set[Sample]]] = Map.empty,
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
