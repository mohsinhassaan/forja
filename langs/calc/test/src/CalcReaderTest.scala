package forja.langs.calc

import utest.*
import forja.Node
import forja.SourceRange
import forja.syntax.*
import forja.TestUtil.assertEqualDiff
import forja.Pass

import CalcReaderTest.*

class CalcReaderTest extends TestSuite:
  def parseString(using path: framework.TestPath)(): Node =
    CalcReader.perform(
      CalcReader.Input.Root(
        CalcReader.Input.ParseHead(),
        SourceRange(path.value.last),
      ),
    )
  end parseString

  val tests = Tests:
    test("popByte") {
      assertEqualDiff(
        CalcReader.readTokens.popByte.pattern
          .runPattern:
            CalcReader.Input
              .Root(
                CalcReader.Input.ParseHead(),
                SourceRange("42"),
              )
              .children
              .asEmptyNodeSpan
        ,
        Some(
          (),
          CalcReader.Input
            .Root(
              CalcReader.Input.ParseHead(),
              '4'.toByte,
              '2'.toByte,
              SourceRange(""),
            )
            .children
            .asEmptyNodeSpan
            .expandRightMax,
        ),
      )
    }

    test("2 + 2") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Number(2),
          CalcReader.Tokenized.Add(),
          CalcReader.Tokenized.Number(2),
        ),
      )
    }
    test("256 +2 - 999* /0") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Number(256),
          CalcReader.Tokenized.Add(),
          CalcReader.Tokenized.Number(2),
          CalcReader.Tokenized.Sub(),
          CalcReader.Tokenized.Number(999),
          CalcReader.Tokenized.Mul(),
          CalcReader.Tokenized.Div(),
          CalcReader.Tokenized.Number(0),
        ),
      )
    }
    test("") {
      assertEqualDiff(parseString(), CalcReader.Tokenized.Root())
    }
    test("    \n\t") {
      assertEqualDiff(parseString(), CalcReader.Tokenized.Root())
    }
    // TODO: invalid char k
    test("5") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Number(5),
        ),
      )
    }
    test("5 + 11") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Number(5),
          CalcReader.Tokenized.Add(),
          CalcReader.Tokenized.Number(11),
        ),
      )
    }
    test("(2 + 3) * 4") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Group(
            CalcReader.Tokenized.Number(2),
            CalcReader.Tokenized.Add(),
            CalcReader.Tokenized.Number(3),
          ),
          CalcReader.Tokenized.Mul(),
          CalcReader.Tokenized.Number(4),
        ),
      )
    }
    test("((2 + 3) * 4)") {
      assertEqualDiff(
        parseString(),
        CalcReader.Tokenized.Root(
          CalcReader.Tokenized.Group(
            CalcReader.Tokenized.Group(
              CalcReader.Tokenized.Number(2),
              CalcReader.Tokenized.Add(),
              CalcReader.Tokenized.Number(3),
            ),
            CalcReader.Tokenized.Mul(),
            CalcReader.Tokenized.Number(4),
          ),
        ),
      )
    }

    test("ModelChecker") {
      modelChecker.assertCheck()
    }
  end tests
end CalcReaderTest

object CalcReaderTest:
  object modelChecker extends Pass.RewritePass.ModelChecker:
    def initTreeLevels: Int = 1
    def initRepMax: Int = 3
    def inputWf: TokenWf = CalcReader.Input.Root
    def outputWf: TokenWf = CalcReader.Tokenized.Root

    export CalcReader.readTokens
    export CalcReader.errorCases

    object genSourceRange extends Pass.RewritePass.EmbedGenerator[SourceRange]:
      val byteBag = IArray[Byte](
        '\n', ' ', '\r', '\t', '+', '-', '*', '/', '(', ')', 'k',
      ) ++ ('0' to '9').map(_.toByte)
      def generate: Iterator[SourceRange] =
        Iterator
          .iterate(List(IArray.empty[Byte])): prefixes =>
            prefixes.flatMap: prefix =>
              byteBag.iterator
                .map(_ +: prefix)
          .takeWhile(_.head.length <= 4)
          .flatten
          .map(SourceRange(_))
      end generate
    end genSourceRange
  end modelChecker
end CalcReaderTest
