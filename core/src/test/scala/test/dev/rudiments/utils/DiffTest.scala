package test.dev.rudiments.utils

import com.github.difflib.DiffUtils
import dev.rudiments.utils.{Chunk, Delta, Diff, Unified}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.jdk.CollectionConverters.*

@RunWith(classOf[JUnitRunner])
class DiffTest extends AnyWordSpec with Matchers {
  private val text1 = Seq("This is a test senctence.", "This is the second line.", "And here is the finish.")
  private val text2 = Seq("This is a test for diffutils.", "This is the second line.")

  "diff wrapping"  in {
    val diff = Diff(text1, text2)

    diff should be(
      Diff(Seq(
        Delta.Change(Chunk(0, Seq("This is a test senctence.")), Chunk(0, Seq("This is a test for diffutils."))),
        Delta.Delete(Chunk(2, Seq("And here is the finish.")))
      ))
    )
  }

  "wrapping should produce similar to lib output" in {
    val diff = Diff(text1, text2)
    diff.applyTo(text1) should be (text2)
  }

  "can make universal diff" in {
    val unified = Unified.generate(text1, "text1", text2, "text2", 0)
    unified should be (Seq(
      "--- text1", "+++ text2",
      "@@ -1,1 +1,1 @@", "-This is a test senctence.", "+This is a test for diffutils.",
      "@@ -3,1 +3,0 @@", "-And here is the finish."
    ))
  }

  "can restore diff from unified format" in {
    val unified = Unified.generate(text1, "text1", text2, "text2", 0)
    Diff.fromUnified(unified) should be (
      Diff(Seq(
        Delta.Change(
          Chunk(0, Seq("This is a test senctence."), Seq(1)),
          Chunk(0, Seq("This is a test for diffutils."), Seq(1))
        ),
        Delta.Change(
          Chunk(2, Seq("And here is the finish."), Seq(3)),
          Chunk(2, Seq.empty[String], Seq.empty) //TODO merge as Delta.Delete
        )
      ))
    )
  }
}
