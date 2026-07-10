package forja

import scala.util.NotGiven
import forja.util.TupleOf

trait Lang:
  final transparent inline given this.type = this
end Lang

object Lang:
  trait Extend[Base <: Lang](using val up: Base) extends Lang

  abstract class Node:
    final transparent inline given this.type = this
    class Retract
    class Replace[OtherNode <: Node] extends Retract

    // Must be defined in a nested object, or _all subclasses_
    // can see T is just Data and typecheck accordingly.
    // That makes a huge mess.
    // That said, X.Scope.T does not look terrible in type annotations.
    object Scope:
      into opaque type T = Data
      def T(data: Data): T = data
      extension (t: T)
        def data: Data = t
      end extension
    end Scope
    export Scope.*

    given Node.TNode.Aux[T, this.type] = new Node.TNode[T]:
      type N = Node.this.type
    end given
  end Node

  object Node:
    sealed trait TNode[T <: Node#T]:
      type N <: Node
    end TNode
    object TNode:
      type Aux[T <: Node#T, N0 <: Node] = TNode[T] {
        type N = N0
      }
    end TNode
  end Node

  abstract class Sum extends Node:
    // Plan: the user implements this type with a sealed class which all implementing productions extend.
    // As a result the compiler will keep track of which values this Sum should accept.
    // To extend, add a nexted Extend class that mashes this Case and its Case together.
    // To shrink, we should take into account C <: Case, C.Retract being present and stop accepting that case.
    type Case <: Node

    // TODO: Sum working at all.
    // Technically it is a Node and so its T and Retract/Replace work, but it is not instantiable.
  end Sum

  abstract class Term[Members <: NamedTuple.AnyNamedTuple] extends Node:
    self: Singleton =>
    
    inline def apply(using inline ng: NotGiven[Retract])(using et: EffectiveType[Members])(members: et.To): T =
      T(Data(this, members.asInstanceOf[Tuple]))
    end apply
  end Term

  final class Data(val term: Term[?], val fields: Tuple)

  sealed trait EffectiveNodeType[From <: Node]:
    type To <: Node
  end EffectiveNodeType
  object EffectiveNodeType:
    type Aux[From <: Node, To0 <: Node] = EffectiveNodeType[From] {
      type To = To0
    }

    def apply[From <: Node, To0 <: Node]: Aux[From, To0] = new EffectiveNodeType[From]:
      type To = To0
    end apply
  end EffectiveNodeType

  given [N <: Node] => (N: N) => NotGiven[N.Retract] => EffectiveNodeType.Aux[N, N] = EffectiveNodeType.apply
  given [N1 <: Node, N2 <: Node] => (N1: N1) => (N1.Replace[N2]) => (et: EffectiveNodeType[N2]) => EffectiveNodeType.Aux[N1, et.To] = EffectiveNodeType.apply

  sealed trait EffectiveType[From]:
    type To
  end EffectiveType
  object EffectiveType:
    type Aux[From, To0] = EffectiveType[From] {
      type To = To0
    }

    def apply[From, To0]: Aux[From, To0] = new EffectiveType[From] {
      type To = To0
    }

    trait Ident[T] extends EffectiveType[T]:
      type To = T
    end Ident
  end EffectiveType

  // Wacky: if T is bounded, implicit search fails. Instead, hack it into trying no matter what T is, and fix the TNode bound using an
  // intersection.
  given [T] => (tn: Node.TNode[T & Node#T]) => (ent: EffectiveNodeType[tn.N]) => (N2: ent.To) => EffectiveType.Aux[T, N2.T] = EffectiveType.apply

  given EffectiveType.Aux[EmptyTuple, EmptyTuple] = EffectiveType.apply
  given [H, Tl <: Tuple] => (eh: EffectiveType[H]) => (et: EffectiveType[Tl]) => EffectiveType.Aux[H *: Tl, eh.To *: (et.To & Tuple)] = EffectiveType.apply

  given [Nt <: NamedTuple.AnyNamedTuple] => (et: EffectiveType[NamedTuple.DropNames[Nt]]) => EffectiveType.Aux[Nt, NamedTuple.NamedTuple[NamedTuple.Names[Nt], et.To & Tuple]] = EffectiveType.apply

  given EffectiveType.Ident[Boolean]
  given EffectiveType.Ident[Byte]
  given EffectiveType.Ident[Char]
  given EffectiveType.Ident[Short]
  given EffectiveType.Ident[Int]
  given EffectiveType.Ident[Long]
  given EffectiveType.Ident[Float]
  given EffectiveType.Ident[Double]

  given EffectiveType.Ident[String]

  given [T] => (et: EffectiveType[T]) => EffectiveType.Aux[Option[T], Option[et.To]] = EffectiveType.apply
  given [T] => (et: EffectiveType[T]) => EffectiveType.Aux[List[T], List[et.To]] = EffectiveType.apply
end Lang

object Test:
  trait L1 extends Lang:
    object Foo extends Lang.Term[(i: Int, j: Int)]
  end L1
  object L1 extends L1

  val x = L1.Foo(i = 42, j = 43)

  trait L2 extends Lang.Extend[L1]:
    export up.{
      Foo as _,
      *,
    }
    object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]

    given up.Foo.Replace[Bar.type] {}
  end L2
  object L2 extends L2

  val y: L2.Bar.T = L2.Bar(s = "hi", opt = Some(L2.Bar("ho", None)))
end Test
