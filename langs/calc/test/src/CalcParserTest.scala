package forja.langs.calc

import utest.*
import forja.SourceRange
import forja.Node

import forja.TestUtil.assertEqualDiff

import forja.syntax.*
import forja.Pass

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

    test("2 + 2") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(CalcAST.Number(2)),
            CalcAST.Expression(CalcAST.Number(2)),
          ),
        ),
      )
    }

    test("1 + 2 + 3") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(CalcAST.Number(1)),
            CalcAST.Expression(
              CalcAST.Add(
                CalcAST.Expression(CalcAST.Number(2)),
                CalcAST.Expression(CalcAST.Number(3)),
              ),
            ),
          ),
        ),
      )
    }

    test("1 + (2 + 3)") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(CalcAST.Number(1)),
            CalcAST.Expression(
              CalcAST.Add(
                CalcAST.Expression(CalcAST.Number(2)),
                CalcAST.Expression(CalcAST.Number(3)),
              ),
            ),
          ),
        ),
      )
    }

    test("(1 + 2) + 3") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(
              CalcAST.Add(
                CalcAST.Expression(CalcAST.Number(1)),
                CalcAST.Expression(CalcAST.Number(2)),
              ),
            ),
            CalcAST.Expression(CalcAST.Number(3)),
          ),
        ),
      )
    }

    test("1 + 2 * 3") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(CalcAST.Number(1)),
            CalcAST.Expression(
              CalcAST.Mul(
                CalcAST.Expression(CalcAST.Number(2)),
                CalcAST.Expression(CalcAST.Number(3)),
              ),
            ),
          ),
        ),
      )
    }

    test("1 * 2 + 3") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(
              CalcAST.Mul(
                CalcAST.Expression(CalcAST.Number(1)),
                CalcAST.Expression(CalcAST.Number(2)),
              ),
            ),
            CalcAST.Expression(CalcAST.Number(3)),
          ),
        ),
      )
    }

    test("5 * 4 + 4 / 2 - 6 * 2") {
      assertEqualDiff(
        parseString(),
        CalcAST.Expression(
          CalcAST.Add(
            CalcAST.Expression(
              CalcAST.Mul(
                CalcAST.Expression(CalcAST.Number(5)),
                CalcAST.Expression(CalcAST.Number(4)),
              ),
            ),
            CalcAST.Expression(
              CalcAST.Sub(
                CalcAST.Expression(
                  CalcAST.Div(
                    CalcAST.Expression(CalcAST.Number(4)),
                    CalcAST.Expression(CalcAST.Number(2)),
                  ),
                ),
                CalcAST.Expression(
                  CalcAST.Mul(
                    CalcAST.Expression(CalcAST.Number(6)),
                    CalcAST.Expression(CalcAST.Number(2)),
                  ),
                ),
              ),
            ),
          ),
        ),
      )
    }

    test("ModelChecker") {
      CalcParserTest.modelChecker.assertCheck()
    }
  end tests
end CalcParserTest

object CalcParserTest:
  object modelChecker extends Pass.RewritePass.ModelChecker:
    def initRepMax: Int = 5
    def initTreeLevels: Int = 2

    export CalcParser.{addParseHead, parseAST, stripMeta}

    def inputWf: TokenWf = CalcParser.reader.Tokenized.Root
    def outputWf: TokenWf = CalcAST.Expression

    object embedInt extends Pass.RewritePass.EmbedGenerator[Int]:
      def generate: Iterator[Int] =
        Iterator(1, 2)
      end generate
    end embedInt
  end modelChecker
end CalcParserTest
