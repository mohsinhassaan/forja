package forja

import utest.*
import forja.syntax.*
import WfTest.*
import forja.TestUtil.assertEqualDiff

class WfTest extends TestSuite:
  extension (node: Node)
    def shouldValidate(tokenWf: TokenWf): Unit =
      val validated = tokenWf.validate.perform(node)
      assertEqualDiff(node, validated)
  end extension

  def tests = Tests:
    test("test1") {
      Test1.Root(Test1.Foo(), Test1.Bar()).shouldValidate(Test1.Root)
    }
    test("test2 bar empty") {
      Test2
        .Root(
          Test2.Foo(),
          Test2.Bar(),
        )
        .shouldValidate(Test2.Root)
    }
    test("test2 bar single") {
      Test2
        .Root(
          Test2.Foo(),
          Test2.Bar(
            Test2.Foo(),
          ),
        )
        .shouldValidate(Test2.Root)
    }
    test("test2 bar double") {
      Test2
        .Root(
          Test2.Foo(),
          Test2.Bar(
            Test2.Foo(),
            Test2.Foo(),
          ),
        )
        .shouldValidate(Test2.Root)
    }
    test("test3 with an int") {
      Test3
        .Root(
          Test3.Foo(),
          Test3.Bar(
            Test3.Foo(),
          ),
          Test3.Ping(42),
        )
        .shouldValidate(Test3.Root)
    }
  end tests
end WfTest

object WfTest:
  trait Test1 extends Wf:
    lazy val Root = Token(
      Foo,
      Bar,
    )
    lazy val Foo = Token()
    lazy val Bar = Token()
  end Test1
  object Test1 extends Test1

  trait Test2 extends Test1:
    override lazy val Bar =
      Test1.Bar.replace(cc.rep(Foo))
  end Test2
  object Test2 extends Test2

  trait Test3 extends Test2:
    override lazy val Root = Test2.Root.replace(
      Foo,
      Bar,
      Ping,
    )
    lazy val Ping = Token(cc.embed[Int])
  end Test3
  object Test3 extends Test3
end WfTest
