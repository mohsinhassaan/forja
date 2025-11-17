package forja

import utest.*

import forja.syntax.*

import NodeSpanTest.*

class NodeSpanTest extends TestSuite:
  def tests = Tests:
    test("slice") {
      val data = (1 to 5).map(cc.lit)
      val span = AST.T(data).children.asNodeSpan

      for
        from <- (0 to data.size + 1)
        until <- (0 to data.size + 1)
      do
        val slicedData = data.slice(from, until)
        val slicedSpan = span.slice(from, until)

        (from, until, slicedSpan.toList) ==> (from, until, slicedData.toList)

        // Do it again, uncovers 0 related assumptions
        for
          from <- (0 to data.size + 1)
          until <- (0 to data.size + 1)
        do
          val slicedData2 = slicedData.slice(from, until)
          val slicedSpan2 = slicedSpan.slice(from, until)

          if slicedSpan2.toList != slicedData2.toList
          then
            Predef.assert(
              false,
              s"$slicedSpan.slice($from, $until) --> $slicedSpan2\n$slicedData.slice($from, $until) --> $slicedData2",
            )
    }
  end tests
end NodeSpanTest

object NodeSpanTest:
  object AST extends Wf:
    lazy val T = Token()
  end AST
end NodeSpanTest
