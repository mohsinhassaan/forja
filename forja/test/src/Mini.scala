package forja.test

import forja.Lang2
import forja.Sum
import forja.Term
import forja.F
import scala.util.NotGiven
import forja.TypeAdjust

object Mini:
  trait L2 extends Lang2:
    class S derives Sum
    object S:
      abstract class Foo derives Term:
        def foo: F[String]
      end Foo
    end S
  end L2

  object L2 extends L2, Lang2.Meta[L3.type]

  trait L3 extends Lang2:
    object up extends L2

    abstract class S derives Sum
    object S:
      export up.S.*
      abstract class Bar derives Term:
        def bar: F[Int]
      end Bar
    end S
  end L3

  object L3 extends L3, Lang2.Meta[L3.type]

  def main(args: Array[String]): Unit =
    import Lang2.{IsNode, LaunderNode, HasSameMeta}
    // erased val a: IsNode.Aux[L2.S, L2.type] = summon[IsNode[L2.S]]
    // erased val b: IsNode.Aux[L2.S.Foo, L2.type] = summon[IsNode[L2.S.Foo]]
    
    // erased val c: LaunderNode.Aux[L3.type, L2.S, L3.S] = summon[LaunderNode[L3.type, L2.S]]
    // erased val d: LaunderNode.Aux[L3.type, L2.S.Foo, L3.S.Foo] = summon[LaunderNode[L3.type, L2.S.Foo]]

    // erased val e = summon[HasSameMeta[L2.S.Foo, L3.S.Foo]]
    // val f = summon[NotGiven[HasSameMeta[L2.S, L3.S]]]

    // summon[Lang2.SumMeta[L2.S]]
    // summon[TypeAdjust.Effective[L2.type, L2.S]]
    L2.sumMeta[L2.S]
    // summon[Lang2.SumMeta[L3.S]]

    // This one fails because it's trying to read terms off the trait L3, not the object
    // erased val e: LaunderNode.Aux[L3, L2.S.Foo, L3.S.Foo] = LaunderNode.instance[L3, L2.S.Foo]

    // erased val a: Lang.EffectiveType.Aux[L.S.Foo.T, L.S.Foo.T] = Lang.effectiveNodeT[L.S.Foo.T]

    // erased val b: Lang.Sum.CaseList.Aux[L.S.type, Tuple1[L.S.Case.Foo.type]] = Lang.Sum.CaseList.instance[L.S.type]

    // erased val c: Lang.Sum.CaseListT.Aux[L.S.T, Tuple1[L.S.Foo.T]] = Lang.Sum.CaseListT.instance[L.S.T]

    // val _ : L.S.Foo.T = L.S.Foo("foo")
    // L.S.Foo("foo") match
    //   case L.S.Foo(f) => println(f)
    // end match
    // val _ : L.Bar.T = L.Bar()
    // L.Bar() match
    //   case L.Bar() => println("ok")
    // end match
    // // val _ : L.S.T = L.S.Foo("foo")
  end main
end Mini
