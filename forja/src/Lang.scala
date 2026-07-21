package forja

import scala.util.NotGiven
import scala.deriving.Mirror
import forja.util.Instanceless
import forja.util.InlineConversion
import scala.annotation.publicInBinary
import scala.compiletime.asMatchable
import java.util.Objects

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

    inline given Node.TNode.Aux[T, this.type] =
      Instanceless[Node.TNode.Aux[T, this.type]]
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
    class HasInner[I <: Sum]
    object Opaque:
      into opaque type T = Any
      extension (t: T)
        transparent inline def ex(using
            inline ng: NotGiven[ReplaceWith[?]],
        )(using et: Sum.EffectiveType[Case])(using
            ei: Sum.EffectiveInnerType[Sum.this.type],
        ): et.T | ei.T =
          t.asInstanceOf
        end ex
      end extension
    end Opaque
    override type T = Opaque.T

    inline given subtype: [T] => (inline ng: NotGiven[ReplaceWith[?]])
      => (et: Sum.EffectiveType[Case])
      => (ei: Sum.EffectiveInnerType[Sum.this.type])
      => InlineConversion.ByCast[et.T | ei.T, this.T] = InlineConversion.ByCast()
  end Sum

  object Sum:
    sealed trait EffectiveInnerType[S <: Sum] extends Instanceless:
      type T
    end EffectiveInnerType
    object EffectiveInnerType:
      type Aux[S <: Sum, T0] = EffectiveInnerType[S] { type T = T0 }

      inline given noInner: [S <: Sum] => (S: S)
        => (inline ng: NotGiven[S.HasInner[?]])
        => Aux[S, Nothing] =
        Instanceless[Aux[S, Nothing]]

      inline given hasInner: [S <: Sum, I <: Sum] => (S: S)
        => S.HasInner[I]
        => (I: I)
        => (et: Sum.EffectiveType[I.Case])
        => (ie: EffectiveInnerType[I])
        => Aux[S, et.T | ie.T] =
        Instanceless[Aux[S, et.T | ie.T]]
    end EffectiveInnerType

    sealed trait EffectiveType[C <: Sum#Case] extends Instanceless:
      type T
    end EffectiveType
    object EffectiveType:
      type Aux[C <: Sum#Case, T0] = EffectiveType[C] {
        type T = T0
      }

      inline given inst: [C <: Sum#Case] => (mirror: Mirror.SumOf[C])
        => (tc: TransformedCases[mirror.MirroredElemTypes])
        => EffectiveType.Aux[C, Tuple.Union[tc.TC]] =
        Instanceless[EffectiveType.Aux[C, Tuple.Union[tc.TC]]]
    end EffectiveType

    sealed trait TransformedCases[Cases <: Tuple] extends Instanceless:
      type TC <: Tuple
    end TransformedCases
    object TransformedCases:
      type Aux[Cases <: Tuple, TC0 <: Tuple] = TransformedCases[Cases] {
        type TC = TC0
      }

      inline def apply[Cases <: Tuple, TC <: Tuple](): Aux[Cases, TC] =
        Instanceless[Aux[Cases, TC]]

      inline given empty: TransformedCases.Aux[EmptyTuple, EmptyTuple] =
        TransformedCases()
      inline given cons: [Hd <: Node, Tl <: Tuple]
        => (Hd: Hd)
        => (inline ng: NotGiven[Hd.Retract])
        => (eht: Lang.EffectiveType[Hd.T])
        => (ttl: TransformedCases[Tl])
        => TransformedCases.Aux[Hd *: Tl, eht.To *: ttl.TC] = TransformedCases()
      inline given consRetracted: [Hd <: Node, Tl <: Tuple]
        => (Hd: Hd)
        => Hd.Retract
        => (ttl: TransformedCases[Tl])
        => TransformedCases.Aux[Hd *: Tl, ttl.TC] = TransformedCases()
    end TransformedCases
  end Sum

  abstract class Term[Members <: NamedTuple.AnyNamedTuple] extends Node:
    self: Singleton =>
    final class T @publicInBinary private[Term] (
        private[Term] val members: Tuple,
    ):
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

    inline def apply(using
        inline ng: NotGiven[ReplaceWith[?]],
    )(using et: EffectiveType[Members])(members: et.To): T =
      T(members.asInstanceOf)
    end apply

    // Patterns and inline do not go well together. Making this inline will create and then call
    // a lambda with the inline body, which is strictly worse than just calling the method.
    // This issue only happens in patterns; the apply above translates to new T properly.
    def unapply(t: T)(using
        ng: NotGiven[ReplaceWith[?]],
    )(using et: EffectiveType[Members]): et.To =
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

    inline def apply[From <: Node, To <: Node]()
        : EffectiveNodeType.Aux[From, To] =
      Instanceless[EffectiveNodeType.Aux[From, To]]
  end EffectiveNodeType

  inline given effectiveNodeIdentity: [N <: Node] => (N: N)
    => (inline ng: NotGiven[N.ReplaceWith[?]]) => EffectiveNodeType.Aux[N, N] =
    EffectiveNodeType()
  inline given effectiveNodeReplaced
      : [N1 <: Node, N2 <: Node] => (N1: N1) => (N1.ReplaceWith[N2])
        => (et: EffectiveNodeType[N2]) => EffectiveNodeType.Aux[N1, et.To] =
    EffectiveNodeType()

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

    inline def apply[From, To](): EffectiveType.Aux[From, To] =
      Instanceless[EffectiveType.Aux[From, To]]
  end EffectiveType

  // Wacky: if T is bounded, implicit search fails. Instead, hack it into trying no matter what T is, and fix the TNode bound using an
  // intersection.
  inline given effectiveNode
      : [T] => (tn: Node.TNode[T & Node#T]) => (ent: EffectiveNodeType[tn.N])
        => (N2: ent.To) => EffectiveType.Aux[T, N2.T] = EffectiveType()

  trait EffectiveTupleType[From <: Tuple]:
    type To <: Tuple
  end EffectiveTupleType
  object EffectiveTupleType:
    type Aux[From <: Tuple, To0 <: Tuple] = EffectiveTupleType[From] {
      type To = To0
    }

    inline def apply[From <: Tuple, To <: Tuple](): Aux[From, To] =
      Instanceless[Aux[From, To]]

    inline given effectiveTupleEmpty: Aux[EmptyTuple, EmptyTuple] =
      EffectiveTupleType()
    inline given effectiveTupleCons: [Hd, Tl <: Tuple]
      => (eh: EffectiveType[Hd]) => (et: EffectiveTupleType[Tl])
      => EffectiveTupleType.Aux[Hd *: Tl, eh.To *: et.To] = EffectiveTupleType()
  end EffectiveTupleType

  inline given effectiveTuple: [T <: Tuple] => (ett: EffectiveTupleType[T])
    => EffectiveType.Aux[T, ett.To] = EffectiveType()
  inline given effectiveNamedTuple: [Nt <: NamedTuple.AnyNamedTuple]
    => (et: EffectiveTupleType[NamedTuple.DropNames[Nt]])
    => EffectiveType.Aux[Nt, NamedTuple.NamedTuple[NamedTuple.Names[
      Nt,
    ], et.To]] = EffectiveType()

  inline given EffectiveType.Ident[Boolean] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Byte] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Char] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Short] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Int] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Long] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Float] = EffectiveType.Ident()
  inline given EffectiveType.Ident[Double] = EffectiveType.Ident()

  inline given EffectiveType.Ident[String] = EffectiveType.Ident()

  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[Option[T], Option[et.To]] = EffectiveType()
  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[List[T], List[et.To]] = EffectiveType()
end Lang

object Test:
  def main(args: Array[String]): Unit =
    trait L1 extends Lang:
      object Foo extends Lang.Term[(i: Int, j: Int)]

      object Ping extends Lang.Sum:
        sealed trait Case extends Lang.Node

        object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
        object Bob extends Lang.Term[(k: Int, foo: Foo.T)], Case
      end Ping
    end L1
    object L1 extends L1

    val x = L1.Foo(i = 42, j = 43)
    println(x)
    val ping: L1.Ping.T = L1.Ping.Pong(k = 12, foo = x)
    println(ping)

    trait L2 extends Lang.Extend[L1]:
      export up.{Foo as _, *}
      object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]

      given r1: up.Foo.ReplaceWith[Bar.type]()
      given r2: up.Ping.Pong.ReplaceWith[Bar.type]()
      // given up.Foo.Retract
      // given up.Ping.Bob.Retract()
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
      // case L2.Ping.Bob(s, opt) =>
      //   println("bob")
    end match

    innerSumTest()
  end main

  def innerSumTest(): Unit =
    trait L1 extends Lang:
      object Foo extends Lang.Term[(i: Int, j: Int)]

      object Ping extends Lang.Sum:
        sealed trait Case extends Lang.Node
        object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
        object Bob extends Lang.Term[(k: Int, foo: Foo.T)], Case
      end Ping
    end L1
    object L1 extends L1

    trait L2 extends Lang.Extend[L1]:
      export up.{Foo as _, Ping as _, *}
      object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]
      object Ping extends Lang.Sum:
        given HasInner[up.Ping.type]()
        export up.Ping.{Case as _, Opaque as _, *}
        sealed trait Case extends Lang.Node
        object NewCase extends Lang.Term[(s: String, opt: Option[up.Foo.T])], Case
      end Ping

      given up.Foo.ReplaceWith[Bar.type]()
      given up.Ping.ReplaceWith[Ping.type]()
    end L2
    object L2 extends L2

    val nc: L2.Ping.T = L2.Ping.NewCase(s = "hello", opt = None)
    println(nc)

    val pong: L2.Ping.T = L2.Ping.Pong(k = 12, foo = L2.Bar(s = "x", opt = None))
    println(pong)

    nc.ex match
      case L2.Ping.NewCase(s, opt) => println(s"NewCase: $s")
      case L2.Ping.Pong(k, foo)   => println(s"Pong: $k")
      case L2.Ping.Bob(k, foo)    => println(s"Bob: $k")
    end match

    pong.ex match
      case L2.Ping.NewCase(s, opt) => println(s"NewCase: $s")
      case L2.Ping.Pong(k, foo)   => println(s"Pong: $k")
      case L2.Ping.Bob(k, foo)    => println(s"Bob: $k")
    end match
  end innerSumTest
end Test
