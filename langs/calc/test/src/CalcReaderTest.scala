package forja.langs.calc

import utest.*
import forja.Node
import forja.SourceRange
import forja.syntax.*

class CalcReaderTest extends TestSuite:
  def parseString(using path: framework.TestPath)(): Node =
    CalcReader(
      CalcReader.Input.Root(
        CalcReader.Input.ParseHead(),
        SourceRange(path.value.last),
      ),
    )
  end parseString

  val tests = Tests:
    test("2 + 2") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Number(2),
        CalcReader.Tokenized.Add(),
        CalcReader.Tokenized.Number(2),
      )
    }
    test("256 +2 - 999* /0") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Number(256),
        CalcReader.Tokenized.Add(),
        CalcReader.Tokenized.Number(2),
        CalcReader.Tokenized.Sub(),
        CalcReader.Tokenized.Number(999),
        CalcReader.Tokenized.Mul(),
        CalcReader.Tokenized.Div(),
        CalcReader.Tokenized.Number(0),
      )
    }
    test("") {
      parseString() ==> CalcReader.Tokenized.Root()
    }
    test("    \n\t") {
      parseString() ==> CalcReader.Tokenized.Root()
    }
    // TODO: invalid char k
    test("5") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Number(5),
      )
    }
    test("5 + 11") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Number(5),
        CalcReader.Tokenized.Add(),
        CalcReader.Tokenized.Number(11),
      )
    }
    test("(2 + 3) * 4") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Group(
          CalcReader.Tokenized.Number(2),
          CalcReader.Tokenized.Add(),
          CalcReader.Tokenized.Number(3),
        ),
        CalcReader.Tokenized.Mul(),
        CalcReader.Tokenized.Number(4),
      )
    }
    test("((2 + 3) * 4)") {
      parseString() ==> CalcReader.Tokenized.Root(
        CalcReader.Tokenized.Group(
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
  end tests
end CalcReaderTest
