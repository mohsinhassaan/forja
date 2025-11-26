package forja.langs.calc

import utest.*
import forja.Pass
import forja.Wf.TokenWf
import forja.SourceRange
import forja.Node
import forja.TestUtil.assertEqualDiff

class CalcEvaluatorTest extends TestSuite:
  def evalString(using path: framework.TestPath)(): Node =
    val ast = CalcParser.perform(
      CalcReader.Input.Root(
        CalcReader.Input.ParseHead(),
        SourceRange(path.value.last),
      ),
    )
    Predef.assert(!ast.containsError, s"parsing error in $ast")
    CalcEvaluator.perform(ast)
  end evalString

  def tests = Tests:
    test("2 + 2") {
      assertEqualDiff(evalString(), CalcAST.Expression(CalcAST.Number(4)))
    }

    test("64 / 0") {
      assertEqualDiff(
        evalString(),
        CalcAST.Expression(
          CalcAST.Div(
            CalcAST.Expression(CalcAST.Number(64)),
            Node.error("division by 0")(
              CalcAST.Expression(CalcAST.Number(0)),
            ),
          ),
        ),
      )
    }

    test("5 * 4 + 4 / 2 - 6 * 2") {
      assertEqualDiff(
        evalString(),
        CalcAST.Expression(CalcAST.Number(5 * 4 + 4 / 2 - 6 * 2)),
      )
    }

    test("ModelChecker") {
      CalcEvaluatorTest.CalcEvaluatorModelChecker.assertCheck()
    }
  end tests
end CalcEvaluatorTest

object CalcEvaluatorTest:
  object CalcEvaluatorModelChecker extends Pass.RewritePass.ModelChecker:
    def initRepMax: Int = 0
    def initTreeLevels: Int = 4
    def inputWf: TokenWf = CalcAST.Expression
    def outputWf: TokenWf = CalcEvaluator.Evaluated.Expression

    export CalcEvaluator.evaluateExpr

    object embedInt extends Pass.RewritePass.EmbedGenerator[Int]:
      def generate: Iterator[Int] =
        Iterator(-1, 0, 1, 2, 42)
    end embedInt
  end CalcEvaluatorModelChecker
end CalcEvaluatorTest
