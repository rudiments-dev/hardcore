package test.dev.rudiments.codecs

import dev.rudiments.codecs.{MJ, MirrorInfo, OneWay, TS}
import dev.rudiments.codecs.Result.*
import dev.rudiments.hardcore.{EdgeTree, Graph, Many}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.compiletime.{constValue, erasedValue, error, summonFrom}
import scala.deriving.Mirror

@RunWith(classOf[JUnitRunner])
class CodecTest extends AnyWordSpec with Matchers {

  "can generate Codecs graph from the Type graph" in {
    //TODO Type Graph with custom types
  }

  "can encode primitive types" in {
    MJ.strToText.t("hello!") should be (Ok(TS.Text("hello!")))
    MJ.intToNumber.t(42) should be (Ok(TS.Number(42)))
  }

  "can encode list of primitive types" in {
    import dev.rudiments.codecs.MJ.given

    MJ.many(using MJ.intToNumber).t(Seq(0, 1, 1, 2, 3, 5)) should be(Ok(TS.Many(Seq(
      TS.Number(0), TS.Number(1), TS.Number(1), TS.Number(2), TS.Number(3), TS.Number(5)
    ))))
  }

  "can encode a map string -> number" in {
    import dev.rudiments.codecs.MJ.given

    MJ.index(using strToText, intToNumber).t(Map(
      "1" -> 1,
      "2" -> 2,
      "42" -> 42,
      "24" -> 24
    )) should be (Ok(TS.Idx(Map(
      TS.Text("1") -> TS.Number(1),
      TS.Text("2") -> TS.Number(2),
      TS.Text("42") -> TS.Number(42),
      TS.Text("24") -> TS.Number(24),
    ))))
  }

  "can derive int and string fields of a case class and recursively" in {
    MirrorInfo[Sample] should be (
      MirrorInfo[Sample]("Sample", Seq("i" -> MirrorInfo.intInfo, "s" -> MirrorInfo.strInfo))
    )

    MirrorInfo[Example] should be(
      MirrorInfo[Example]("Example", Seq(
        "i" -> MirrorInfo.intInfo,
        "s" -> MirrorInfo[Sample]("Sample", Seq("i" -> MirrorInfo.intInfo, "s" -> MirrorInfo.strInfo))
      ))
    )
  }
}
