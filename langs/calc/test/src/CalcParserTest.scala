package forja.langs.calc

import utest.*
import forja.SourceRange
import forja.Node

import forja.TestUtil.assertEqualDiff

import forja.syntax.*

class CalcParserTest extends TestSuite:
  def parseString(using path: framework.TestPath)(): Node =
    CalcParser.perform(
      CalcReader.Input.Root(
        CalcReader.Input.ParseHead(),
        SourceRange(path.value.last),
      ),
    )
  end parseString

  def tests = Tests:
    test("42") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Number(42),
        ),
      )
    }

    // test("2 + 2") {
    //   assertEqualDiff(parseString(), CalcAST.Expression(
    //     CalcAST.Add(
    //       CalcAST.Expression(CalcAST.Number(2)),
    //       CalcAST.Expression(CalcAST.Number(2)),
    //     ),
    //   ))
    // }

    // test("1 + 2 + 3") {
    //   parseString() ==> CalcAST.Expression(
    //     CalcAST.Add(
    //       CalcAST.Expression(
    //         CalcAST.Add(
    //           CalcAST.Expression(CalcAST.Number(1)),
    //           CalcAST.Expression(CalcAST.Number(2)),
    //         ),
    //       ),
    //       CalcAST.Expression(CalcAST.Number(3)),
    //     ),
    //   )
    // }

    // test("1 + (2 + 3)") {
    //   parseString() ==> CalcAST.Expression(
    //     CalcAST.Add(
    //       CalcAST.Expression(CalcAST.Number(1)),
    //       CalcAST.Expression(
    //         CalcAST.Add(
    //           CalcAST.Expression(CalcAST.Number(2)),
    //           CalcAST.Expression(CalcAST.Number(3)),
    //         ),
    //       ),
    //     ),
    //   )
    // }
  end tests
end CalcParserTest
