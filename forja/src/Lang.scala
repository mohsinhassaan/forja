package forja

import scala.util.NotGiven
import scala.reflect.TypeTest
import scala.annotation.publicInBinary
import scala.deriving.Mirror
import forja.util.Instanceless
import forja.util.InlineConversion
import java.util.Objects
import scala.compiletime.asMatchable

trait Lang:
  final transparent inline given this.type = this
end Lang

object Lang:
  trait Extend[Base <: Lang](using val up: Base) extends Lang

  abstract class Node:
    final transparent inline given this.type = this
    class ReplaceWith[OtherNode <: Node]
    class Retract extends ReplaceWith[Node.Empty.type]

    type T

    inline given Node.TNode.Aux[T, this.type] = Instanceless[Node.TNode.Aux[T, this.type]]
  end Node

  object Node:
    sealed trait TNode[T <: Node#T] extends Instanceless:
      type N <: Node
    end TNode
    object TNode:
      type Aux[T <: Node#T, N0 <: Node] = TNode[T] {
        type N = N0
      }
    end TNode

    object Empty extends Node:
      type T = Nothing
    end Empty
  end Node

  abstract class Sum extends Node:
    type Case <: Node
    object Opaque:
      into opaque type T = Any
      extension (t: T)
        inline def ex(using inline ng: NotGiven[ReplaceWith[?]])(using et: Sum.EffectiveType[Case]): et.T =
          t.asInstanceOf
        end ex
      end extension
    end Opaque
    override type T = Opaque.T

    inline given subtype: [T] => (inline ng: NotGiven[ReplaceWith[?]]) => (et: Sum.EffectiveType[Case]) => InlineConversion.ByCast[et.T, this.T] = InlineConversion.ByCast()
  end Sum

  object Sum:
    sealed trait EffectiveType[C <: Sum#Case] extends Instanceless:
      type T <: Node#T
    end EffectiveType
    object EffectiveType:
      type Aux[C <: Sum#Case, T0 <: Node#T] = EffectiveType[C] {
        type T = T0
      }

      inline given inst: [C <: Sum#Case] => (mirror: Mirror.SumOf[C]) => (tc: TransformedCases[mirror.MirroredElemTypes]) => EffectiveType.Aux[C, tc.C] = Instanceless[EffectiveType.Aux[C, tc.C]]
    end EffectiveType

    sealed trait TransformedCases[Cases <: Tuple] extends Instanceless:
      type C <: Node#T
    end TransformedCases
    object TransformedCases:
      type Aux[Cases <: Tuple, C0 <: Node#T] = TransformedCases[Cases] {
        type C = C0
      }

      inline def apply[Cases <: Tuple, C <: Node#T](): Aux[Cases, C] = Instanceless[Aux[Cases, C]]

      inline given empty: TransformedCases.Aux[EmptyTuple, Nothing] = TransformedCases()
      inline given cons: [Hd, Tl <: Tuple] => (eht: EffectiveNodeType[Hd & Node]) => (HN: eht.To) => (ttl: TransformedCases[Tl]) => TransformedCases.Aux[Hd *: Tl, HN.T | ttl.C] = TransformedCases()
    end TransformedCases
  end Sum

  abstract class Term[Members <: NamedTuple.AnyNamedTuple] extends Node:
    self: Singleton =>
    final class T @publicInBinary private[Term] (private[Term] val members: Tuple):
      override def toString(): String = s"${self.getClass().getName()}$members"
      override def equals(obj: Any): Boolean =
        obj.asMatchable match
          case other: T => members == other.members
        end match
      end equals
      override def hashCode(): Int =
        Objects.hash(self, members)
      end hashCode
    end T
    
    inline def apply(using inline ng: NotGiven[ReplaceWith[?]])(using et: EffectiveType[Members])(members: et.To): T =
      T(members.asInstanceOf)
    end apply

    // Patterns and inline do not go well together. Making this inline will create and then call
    // a lambda with the inline body, which is strictly worse than just calling the method.
    // This issue only happens in patterns; the apply above translates to new T properly.
    def unapply(t: T)(using ng: NotGiven[ReplaceWith[?]])(using et: EffectiveType[Members]): et.To =
      t.members.asInstanceOf[et.To]
    end unapply
  end Term

  sealed trait EffectiveNodeType[From <: Node] extends Instanceless:
    type To <: Node
  end EffectiveNodeType
  object EffectiveNodeType:
    type Aux[From <: Node, To0 <: Node] = EffectiveNodeType[From] {
      type To = To0
    }

    inline def apply[From <: Node, To <: Node](): EffectiveNodeType.Aux[From, To] = Instanceless[EffectiveNodeType.Aux[From, To]]
  end EffectiveNodeType

  inline given effectiveNodeIdentity: [N <: Node] => (N: N) => (inline ng: NotGiven[N.ReplaceWith[?]]) => EffectiveNodeType.Aux[N, N] = EffectiveNodeType()
  inline given effectiveNodeReplaced: [N1 <: Node, N2 <: Node] => (N1: N1) => (N1.ReplaceWith[N2]) => (et: EffectiveNodeType[N2]) => EffectiveNodeType.Aux[N1, et.To] = EffectiveNodeType()

  sealed trait EffectiveType[From] extends Instanceless:
    type To
  end EffectiveType
  object EffectiveType:
    type Aux[From, To0] = EffectiveType[From] {
      type To = To0
    }

    trait Ident[T] extends EffectiveType[T]:
      type To = T
    end Ident
    object Ident:
      inline def apply[T](): Ident[T] = Instanceless[Ident[T]]
    end Ident

    inline def apply[From, To](): EffectiveType.Aux[From, To] = Instanceless[EffectiveType.Aux[From, To]]
  end EffectiveType

  // Wacky: if T is bounded, implicit search fails. Instead, hack it into trying no matter what T is, and fix the TNode bound using an
  // intersection.
  inline given effectiveNode: [T] => (tn: Node.TNode[T & Node#T]) => (ent: EffectiveNodeType[tn.N]) => (N2: ent.To) => EffectiveType.Aux[T, N2.T] = EffectiveType()

  trait EffectiveTupleType[From <: Tuple]:
    type To <: Tuple
  end EffectiveTupleType
  object EffectiveTupleType:
    type Aux[From <: Tuple, To0 <: Tuple] = EffectiveTupleType[From] {
      type To = To0
    }

    inline def apply[From <: Tuple, To <: Tuple](): Aux[From, To] = Instanceless[Aux[From, To]]

    inline given effectiveTupleEmpty: Aux[EmptyTuple, EmptyTuple] = EffectiveTupleType()
    inline given effectiveTupleCons: [Hd, Tl <: Tuple] => (eh: EffectiveType[Hd]) => (et: EffectiveTupleType[Tl]) => EffectiveTupleType.Aux[Hd *: Tl, eh.To *: et.To] = EffectiveTupleType()
  end EffectiveTupleType

  inline given effectiveTuple: [T <: Tuple] => (ett: EffectiveTupleType[T]) => EffectiveType.Aux[T, ett.To] = EffectiveType()
  inline given effectiveNamedTuple: [Nt <: NamedTuple.AnyNamedTuple] => (et: EffectiveTupleType[NamedTuple.DropNames[Nt]]) => EffectiveType.Aux[Nt, NamedTuple.NamedTuple[NamedTuple.Names[Nt], et.To]] = EffectiveType()

  inline given EffectiveType.Ident[Boolean] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Byte] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Char] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Short] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Int] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Long] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Float] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Double] = EffectiveType.Ident()

  inline given EffectiveType.Ident[String] = EffectiveType.Ident()

  inline given [T] => (et: EffectiveType[T]) => EffectiveType.Aux[Option[T], Option[et.To]] = EffectiveType()
  inline given [T] => (et: EffectiveType[T]) => EffectiveType.Aux[List[T], List[et.To]] = EffectiveType()
end Lang

object Test:
  def main(args: Array[String]): Unit =
    trait L1 extends Lang:
      object Foo extends Lang.Term[(i: Int, j: Int)]

      object Ping extends Lang.Sum:
        sealed trait Case extends Lang.Node

        object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
      end Ping
    end L1
    object L1 extends L1

    val x = L1.Foo(i = 42, j = 43)
    println(x)
    val ping: L1.Ping.T = L1.Ping.Pong(k = 12, foo = x)
    println(ping)

    trait L2 extends Lang.Extend[L1]:
      export up.{
        Foo as _,
        *,
      }
      object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]

      given r1: up.Foo.ReplaceWith[Bar.type]()
      given r2: up.Ping.Pong.ReplaceWith[Bar.type]()
      // given up.Foo.Retract
    end L2
    object L2 extends L2

    val y: L2.Bar.T = L2.Bar(s = "hi", opt = Some(L2.Bar("ho", None)))
    println(y)
    val ping2: L2.Ping.T = y
    println(ping2)

    y match
      case L2.Bar(s, opt) =>
        println((s, opt))
    end match

    ping2.ex match
      case L2.Bar(s, opt) =>
        println(s"$s, $opt")
    end match
  end main
end Test
