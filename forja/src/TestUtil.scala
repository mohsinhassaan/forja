package forja

import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import scala.jdk.CollectionConverters.given

object TestUtil:
  def diffView[T](original: T, result: T): String =
    val originalLines = original.toString().linesIterator.toBuffer.asJava
    val patch = DiffUtils.diff(
      originalLines,
      result.toString().linesIterator.toBuffer.asJava,
    )
    val diffLines = UnifiedDiffUtils.generateUnifiedDiff(
      "original",
      "result",
      originalLines,
      patch,
      5,
    )
    diffLines.asScala.mkString("\n")
  end diffView

  def assertEqualDiff[T](original: T, result: T): Unit =
    assert(original == result, diffView(original, result))
  end assertEqualDiff
end TestUtil
