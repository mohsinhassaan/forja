package forja

import utest.*

import forja.syntax.*
import PatternTest.*

class PatternTest extends TestSuite:
  extension (node: Node)
    def rwChildren[T <: Matchable](rw: Query.rewrite[T]): Node =
      rw.pattern
        .runPattern(node.children.asEmptyNodeSpan)
        .map(_._2)
        .flatMap(_.parentOption)
        .get
    end rwChildren
  end extension

  def tests = Tests:
    test("single") {
      AST
        .Root()
        .rwChildren:
          on().rewrite(_ => NodeSpan(42))
      ==> AST.Root(42)

      AST
        .Root(
          AST.T1(),
        )
        .rwChildren:
          on(AST.T1()).rewrite(_ => NodeSpan(AST.T2()))
      ==> AST.Root(AST.T2())

      AST
        .Root(
          42,
        )
        .rwChildren:
          on(cc.lit(42)).rewrite(_ => NodeSpan(43))
      ==> AST.Root(43)
    }

    test("reps") {
      AST
        .Root()
        .rwChildren:
          on(cc.rep(cc.lit(42))).rewrite(_ => NodeSpan())
      ==> AST.Root()

      AST
        .Root(
          AST.T1(),
        )
        .rwChildren:
          on(cc.rep(AST.T1())).rewrite(_ => NodeSpan())
      ==> AST.Root()

      AST
        .Root(
          AST.T1(),
          AST.T1(),
        )
        .rwChildren:
          on(cc.rep(AST.T1())).rewrite(_ => NodeSpan())
      ==> AST.Root()

      AST
        .Root(
          AST.T1(),
          AST.T1(),
          AST.T2(),
          AST.T1(),
        )
        .rwChildren:
          on(cc.rep(AST.T1())).rewrite(_ => NodeSpan())
      ==> AST.Root(
        AST.T2(),
        AST.T1(),
      )
    }

    test("nested") {
      AST
        .Root(
          42,
        )
        .rwChildren:
          on(
            cc.lit(42).rewrite(_ => cc.lit(43)),
          ).rewriteInPattern
      ==> AST.Root(43)

      AST
        .Root(
          AST.T1(
            42,
          ),
        )
        .rwChildren:
          on(
            AST.T1(
              cc.lit(42).rewrite(_ => cc.lit(43)),
            ),
          ).rewriteInPattern
      ==> AST.Root(AST.T1(43))

      AST
        .Root(
          AST.T1(1),
          AST.T2(2),
        )
        .rwChildren:
          on(
            AST.T1(`...`),
            AST.T2(+cc.embed[Int]).rewrite(i => NodeSpan(i)),
          ).rewriteInPattern
      ==> AST.Root(AST.T1(1), 2)
    }

    test("captures") {
      AST
        .Root(
          AST.T1(-1),
          AST.T1(42),
        )
        .rwChildren:
          on(
            !AST.T1(cc.lit(-1)),
            +AST.T1(+cc.embed[Int]),
          ).rewrite: (t1, i) =>
            NodeSpan(i, t1)
      ==> AST.Root(42, AST.T1(-1))
    }

    test("wildcards") {
      AST
        .Root(
          1, 2, 3, 4, 5,
        )
        .rwChildren:
          on(
            +cc.lit(1),
            `...`,
            +cc.lit(5),
          ).rewrite: (i1, i2) =>
            NodeSpan(i1, i2)
      ==> AST.Root(1, 5)

      AST
        .Root(
          1, 2, 3, 4, 5,
        )
        .rwChildren:
          on(
            cc.lit(1),
            `...`,
            +cc.embed[Int],
            cc.lit(5),
          ).rewrite: i =>
            NodeSpan(i)
      ==> AST.Root(4)

      AST
        .Root(
          1, 2, 3, 4, 5,
        )
        .rwChildren:
          on(
            `...`,
            cc.lit(2),
            +cc.embed[Int],
            `...`,
            +cc.embed[Int],
            cc.lit(5),
          ).rewrite: (i1, i2) =>
            NodeSpan(i1, i2)
      ==> AST.Root(3, 4)
    }
  end tests
end PatternTest

object PatternTest:
  trait AST extends Wf:
    lazy val Root = Token()
    lazy val T1 = Token()
    lazy val T2 = Token()
  end AST
  object AST extends AST
end PatternTest
