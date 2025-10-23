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
  end tests
end CalcReaderTest
