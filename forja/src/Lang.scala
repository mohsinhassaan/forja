package forja

import scala.util.NotGiven
import scala.deriving.Mirror
import forja.util.InlineConversion
import scala.annotation.publicInBinary
import scala.compiletime.asMatchable
import java.util.Objects
import scala.compiletime.Erased
import scala.compiletime.summonInline
import forja.util.TrivialMatch

trait Lang:
  final transparent inline given this.type = this
end Lang

object Lang:
  trait Extend[Base <: Lang](using val up: Base) extends Lang

  sealed abstract class NodeT[N <: Node] extends Erased:
    type T
  end NodeT

  object NodeT:
    final class Launder[N <: Node, T]
    object Launder:
      given instance: [N <: Node] => (N: N) => Launder[N, N.T] =
        new Launder
      end instance
    end Launder

    final class Aux[N <: Node, T0] extends NodeT[N]:
      type T = T0
    end Aux

    inline given instance
        : [N <: Node, T] => (inline ev: Launder[N, T]) => Aux[N, T] =
      new Aux
    end instance
  end NodeT

  sealed abstract class TNode[T] extends Erased:
    type N <: Node
  end TNode

  object TNode:
    final class Launder[N <: Node, T]

    final class Aux[N0 <: Node, T] extends TNode[T]:
      type N = N0
    end Aux

    inline given instance
        : [N <: Node, T] => (inline ev: Launder[N, T]) => Aux[N, T] =
      new Aux
    end instance
  end TNode

  final class NodeRetract[N <: Node] extends Erased

  object NodeRetract:
    final class Launder[N <: Node]

    object Launder:
      inline given instance: [N <: Node] => (N: N) => N.Retract => Launder[N] =
        new Launder
    end Launder

    inline given instance
        : [N <: Node] => (inline ev: Launder[N]) => NodeRetract[N] =
      new NodeRetract
    end instance
  end NodeRetract

  sealed abstract class NodeReplaceWith[N1 <: Node] extends Erased:
    type OtherNode <: Node
  end NodeReplaceWith

  object NodeReplaceWith:
    final class Aux[N <: Node, OtherNode0 <: Node] extends NodeReplaceWith[N]:
      type OtherNode = OtherNode0
    end Aux

    final class Launder[N <: Node, OtherNode <: Node]

    object Launder:
      inline given instance: [N <: Node, OtherNode <: Node] => (N: N)
        => N.ReplaceWith[OtherNode] => Launder[N, OtherNode] =
        new Launder
      end instance
    end Launder

    inline given instance: [N <: Node, OtherNode <: Node]
      => (inline ev: Launder[N, OtherNode]) => Aux[N, OtherNode] =
      new Aux
    end instance
  end NodeReplaceWith

  abstract class Node:
    final transparent inline given this.type = this
    class ReplaceWith[OtherNode <: Node]
    open class Retract extends ReplaceWith[Node.Empty.type]

    type T

    inline given TNode.Launder[this.type, T] = new TNode.Launder
  end Node

  object Node:
    object Empty extends Node:
      type T = Nothing
    end Empty
  end Node

  abstract class Sum extends Node:
    sum =>
    type Case <: Node
    object Opaque:
      into opaque type T = Any
      extension (t: T)
        transparent inline def ex(using
            inline ng: NotGiven[ReplaceWith[?]],
        )(using et: Sum.EffectiveType[Sum.this.type]): et.T =
          // because it will claim unexhaustive no matter what as of writing :(
          t.asInstanceOf[et.T].runtimeChecked
        end ex
      end extension
    end Opaque
    override type T = Opaque.T

    abstract class Extends extends Sum, Selectable:
      final type Super = sum.type
      final inline given InlineConversion.ByCast[T, sum.T] =
        new InlineConversion.ByCast
    end Extends
  end Sum

  object Sum:
    // Intentionally over-broad conversion so we see error messages from the summons inside it, not the generic inapplicable conversion message (that is, act like there is no conversion)
    inline given subtypeConversionPoint: [T, U, ET]
      => (inline ng: NotGiven[T =:= U]) => (tn: TNode[T]) => (un: TNode[U])
      => (unet: EffectiveType[un.N]) => (unet.T =:= ET)
      => InlineConversion.ByCast[T, U] =
      // Not sure how useful this is, but it's a sanity check to ensure no one is trying to use a replaced Sum instead of its replacement.
      summonInline[NotGiven[NodeReplaceWith[un.N]]]
      // Extra type param ET is used to "encourage" the compiler to print the actual type, not a term reference to part of our inline elaboration,
      // in case this summon fails.
      summonInline[T <:< ET]
      new InlineConversion.ByCast[T, U]
    end subtypeConversionPoint

    sealed abstract class SMirror[S <: Sum] extends Erased:
      type Cases <: Tuple
    end SMirror

    object SMirror:
      final class Aux[S <: Sum, Cases0 <: Tuple] extends SMirror[S]:
        type Cases = Cases0
      end Aux

      final class Launder[S <: Sum, Cases <: Tuple]

      object Launder:
        given instance: [S <: Sum] => (S: S) => (mirror: Mirror.SumOf[S.Case])
          => Launder[S, mirror.MirroredElemTypes] = new Launder
      end Launder

      inline given instance: [S <: Sum, Cases <: Tuple]
        => (inline ev: Launder[S, Cases]) => Aux[S, Cases] =
        new Aux
      end instance
    end SMirror

    sealed abstract class SSuper[S <: Sum#Extends] extends Erased:
      type Super <: Sum
    end SSuper

    object SSuper:
      final class Aux[S <: Sum#Extends, Super0 <: Sum] extends SSuper[S]:
        type Super = Super0
      end Aux

      final class Launder[S <: Sum#Extends, Super]

      object Launder:
        given instance: [S <: Sum#Extends] => (S: S) => Launder[S, S.Super] =
          new Launder
      end Launder

      inline given instance: [S <: Sum#Extends, Super <: Sum]
        => (inline ev: Launder[S, Super]) => Aux[S, Super] =
        new Aux
      end instance
    end SSuper

    sealed abstract class EffectiveType[S] extends Erased:
      type T
    end EffectiveType

    object EffectiveType:
      final class Aux[S <: Sum, T0] extends EffectiveType[S]:
        type T = T0
      end Aux

      inline given inst: [S <: Sum] => (list: EffectiveTypeList[S])
        => (proj: EffectiveTypeProjection[list.Cases])
        => Aux[S, Tuple.Union[proj.T]] =
        new Aux
      end inst

      sealed abstract class EffectiveTypeProjection[Tpl <: Tuple]
          extends Erased:
        type T <: Tuple
      end EffectiveTypeProjection

      object EffectiveTypeProjection:
        final class Aux[Tpl <: Tuple, T0 <: Tuple]
            extends EffectiveTypeProjection[Tpl]:
          type T = T0
        end Aux

        inline given empty: Aux[EmptyTuple, EmptyTuple] = new Aux

        inline given cons: [Hd <: Node, Tl <: Tuple] => (Hd: NodeT[Hd])
          => (rec: EffectiveTypeProjection[Tl])
          => Aux[Hd *: Tl, Hd.T *: rec.T] = new Aux
      end EffectiveTypeProjection
    end EffectiveType

    sealed abstract class EffectiveTypeList[S <: Sum] extends Erased:
      type Cases <: Tuple
    end EffectiveTypeList

    object EffectiveTypeList:
      final class Aux[S <: Sum, Cases0 <: Tuple] extends EffectiveTypeList[S]:
        type Cases = Cases0
      end Aux

      inline given instBase: [S <: Sum]
        => (inline ng: NotGiven[S <:< Sum#Extends]) => (mirror: SMirror[S])
        => (tc: TransformedCases[mirror.Cases])
        => Aux[S, tc.TC] =
        new Aux
      end instBase

      inline given instExtends: [S <: Sum#Extends] => (S: SSuper[S])
        => (rec: EffectiveTypeList[S.Super]) => (mirror: SMirror[S])
        => (tc: TransformedCases[mirror.Cases])
        => Aux[S, Tuple.Concat[rec.Cases, tc.TC]] =
        new Aux
      end instExtends

      inline given instExtendsEmpty: [S <: Sum#Extends]
        => (inline ng: NotGiven[SMirror[S]])
        => (S: SSuper[S])
        => (rec: EffectiveTypeList[S.Super])
        => Aux[S, rec.Cases] =
        new Aux
      end instExtendsEmpty
    end EffectiveTypeList

    sealed abstract class TransformedCases[Cases <: Tuple] extends Erased:
      type TC <: Tuple
    end TransformedCases

    object TransformedCases:
      final class Aux[Cases <: Tuple, TC0 <: Tuple]
          extends TransformedCases[Cases]:
        type TC = TC0
      end Aux

      inline given empty: Aux[EmptyTuple, EmptyTuple] = new Aux

      inline given cons: [Hd <: Node, Tl <: Tuple]
        => (inline ng: NotGiven[NodeRetract[Hd]])
        => (eht: Lang.EffectiveNodeType[Hd])
        => (ttl: TransformedCases[Tl])
        => Aux[Hd *: Tl, eht.To *: ttl.TC] =
        new Aux
      end cons

      inline given consRetracted: [Hd <: Node, Tl <: Tuple]
        => NodeRetract[Hd]
        => (ttl: TransformedCases[Tl])
        => Aux[Hd *: Tl, ttl.TC] =
        new Aux
      end consRetracted
    end TransformedCases
  end Sum

  abstract class Term[Members <: NamedTuple.AnyNamedTuple] extends Node:
    self: Singleton =>
    type MembersType = Members
    final class T @publicInBinary private[Lang] (
        private[Lang] val members: Tuple,
    ):
      private[Lang] val _outer: AnyRef = self.asInstanceOf[AnyRef]
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

    private[Lang] def isMyT(v: Any): Boolean =
      v.isInstanceOf[T] && v
        .asInstanceOf[T]
        ._outer
        .eq(self.asInstanceOf[AnyRef])

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
    )(using et: EffectiveType[Members]): TrivialMatch[et.To] =
      TrivialMatch(t.members.asInstanceOf[et.To])
    end unapply
  end Term

  sealed abstract class EffectiveNodeType[From <: Node] extends Erased:
    type To <: Node
  end EffectiveNodeType

  object EffectiveNodeType:
    final class Aux[From <: Node, To0 <: Node] extends EffectiveNodeType[From]:
      type To = To0
    end Aux
  end EffectiveNodeType

  inline given effectiveNodeIdentity: [N <: Node]
    => (inline ng: NotGiven[NodeReplaceWith[N]])
    => EffectiveNodeType.Aux[N, N] =
    new EffectiveNodeType.Aux
  inline given effectiveNodeReplaced: [N <: Node] => (r: NodeReplaceWith[N])
    => (et: EffectiveNodeType[r.OtherNode]) => EffectiveNodeType.Aux[N, et.To] =
    new EffectiveNodeType.Aux

  sealed abstract class EffectiveType[From] extends Erased:
    type To
  end EffectiveType

  object EffectiveType:
    sealed class Aux[From, To0] extends EffectiveType[From]:
      type To = To0
    end Aux

    final class Ident[T] extends Aux[T, T]
  end EffectiveType

  inline given effectiveNode
      : [T] => (tn: TNode[T]) => (ent: EffectiveNodeType[tn.N])
        => (N2: NodeT[ent.To]) => EffectiveType.Aux[T, N2.T] =
    new EffectiveType.Aux

  sealed abstract class EffectiveTupleType[From <: Tuple] extends Erased:
    type To <: Tuple
  end EffectiveTupleType

  object EffectiveTupleType:
    final class Aux[From <: Tuple, To0 <: Tuple]
        extends EffectiveTupleType[From]:
      type To = To0
    end Aux

    inline given effectiveTupleEmpty: Aux[EmptyTuple, EmptyTuple] =
      new Aux
    inline given effectiveTupleCons: [Hd, Tl <: Tuple]
      => (eh: EffectiveType[Hd]) => (et: EffectiveTupleType[Tl])
      => Aux[Hd *: Tl, eh.To *: et.To] = new Aux
  end EffectiveTupleType

  inline given effectiveTuple: [T <: Tuple] => (ett: EffectiveTupleType[T])
    => EffectiveType.Aux[T, ett.To] = new EffectiveType.Aux
  inline given effectiveNamedTuple: [Nt <: NamedTuple.AnyNamedTuple]
    => (et: EffectiveTupleType[NamedTuple.DropNames[Nt]])
    => EffectiveType.Aux[Nt, NamedTuple.NamedTuple[NamedTuple.Names[
      Nt,
    ], et.To]] = new EffectiveType.Aux

  inline given EffectiveType.Ident[Boolean] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Byte] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Char] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Short] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Int] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Long] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Float] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Double] = new EffectiveType.Ident

  inline given EffectiveType.Ident[String] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Unit] = new EffectiveType.Ident

  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[Option[T], Option[et.To]] = new EffectiveType.Aux
  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[List[T], List[et.To]] = new EffectiveType.Aux

  // transform

  abstract class Rewrite[From, To]:
    def rewrite(t: From): To
  end Rewrite

  sealed abstract class TermMembers[N <: Term[?]] extends Erased:
    type Members <: NamedTuple.AnyNamedTuple
  end TermMembers

  object TermMembers:
    final class Aux[N <: Term[?], M <: NamedTuple.AnyNamedTuple]
        extends TermMembers[N]:
      type Members = M
    end Aux

    final class Launder[N <: Term[?], M <: NamedTuple.AnyNamedTuple]

    object Launder:
      given instance: [N <: Term[?]] => (N: N) => Launder[N, N.MembersType] =
        new Launder
    end Launder

    inline given instance: [N <: Term[?], M <: NamedTuple.AnyNamedTuple]
      => (inline ev: Launder[N, M]) => Aux[N, M] =
      new Aux
  end TermMembers

  abstract class NodeClassTag[N <: Node]:
    def isInstance(v: Any): Boolean
  end NodeClassTag

  object NodeClassTag:
    final class Launder[N <: Node](val node: N)

    object Launder:
      given instance: [N <: Node] => (N: N) => Launder[N] =
        new Launder(N)
    end Launder

    given forTerm: [N <: Term[?]] => (ev: Launder[N])
      => NodeClassTag[N] =
      val n = ev.node
      new NodeClassTag[N]:
        def isInstance(v: Any): Boolean = n.isMyT(v)
  end NodeClassTag

  abstract class OptionalRewrite[T]:
    def applyIfPresent(t: Any): Any
  end OptionalRewrite

  object OptionalRewrite:
    given withRewrite: [T] => (rw: Rewrite[T, ?]) => OptionalRewrite[T] =
      new OptionalRewrite[T]:
        def applyIfPresent(t: Any): Any = rw.rewrite(t.asInstanceOf[T])

    given withoutRewrite: [T] => (ng: NotGiven[Rewrite[T, ?]])
      => OptionalRewrite[T] =
      new OptionalRewrite[T]:
        def applyIfPresent(t: Any): Any = t
  end OptionalRewrite

  abstract class SumOptionalRewrite[S <: Sum]:
    def applyOr(t: Any, fallback: () => Any): Any
  end SumOptionalRewrite

  object SumOptionalRewrite:
    final class Launder[S <: Sum](val rw: Any => Any)

    object Launder:
      given withRewrite: [S <: Sum] => (S: S) => (rw: Rewrite[S.T, ?])
        => Launder[S] =
        new Launder(t => rw.rewrite(t.asInstanceOf[S.T]).asInstanceOf[Any])
    end Launder

    given withRewrite: [S <: Sum] => (ev: Launder[S])
      => SumOptionalRewrite[S] =
      val f = ev.rw
      new SumOptionalRewrite[S]:
        def applyOr(t: Any, fallback: () => Any): Any = f(t)

    given withoutRewrite: [S <: Sum] => (ng: NotGiven[Launder[S]])
      => SumOptionalRewrite[S] =
      new SumOptionalRewrite[S]:
        def applyOr(t: Any, fallback: () => Any): Any = fallback()
  end SumOptionalRewrite

  abstract class TransformField[T]:
    def transformField(t: T): T
  end TransformField

  object TransformField:
    given TransformField[Boolean]:
      def transformField(t: Boolean): Boolean = t
    given TransformField[Byte]:
      def transformField(t: Byte): Byte = t
    given TransformField[Char]:
      def transformField(t: Char): Char = t
    given TransformField[Short]:
      def transformField(t: Short): Short = t
    given TransformField[Int]:
      def transformField(t: Int): Int = t
    given TransformField[Long]:
      def transformField(t: Long): Long = t
    given TransformField[Float]:
      def transformField(t: Float): Float = t
    given TransformField[Double]:
      def transformField(t: Double): Double = t
    given TransformField[String]:
      def transformField(t: String): String = t
    given TransformField[Unit]:
      def transformField(t: Unit): Unit = t

    given nodeField: [T] => (tn: TNode[T]) => (ent: EffectiveNodeType[tn.N])
      => (xform: => Transform[ent.To, ?]) => TransformField[T] =
      new TransformField[T]:
        def transformField(t: T): T =
          xform.transformAny(t.asInstanceOf[Any]).asInstanceOf[T]

    given optionField
        : [T] => (inner: TransformField[T]) => TransformField[Option[T]] =
      new TransformField[Option[T]]:
        def transformField(t: Option[T]): Option[T] =
          t.map(inner.transformField)

    given listField
        : [T] => (inner: TransformField[T]) => TransformField[List[T]] =
      new TransformField[List[T]]:
        def transformField(t: List[T]): List[T] =
          t.map(inner.transformField)
  end TransformField

  abstract class TransformTuple[Tpl <: Tuple]:
    def transformTuple(t: Tuple): Tuple
  end TransformTuple

  object TransformTuple:
    given empty: TransformTuple[EmptyTuple]:
      def transformTuple(t: Tuple): Tuple = EmptyTuple

    given cons: [Hd, Tl <: Tuple] => (hd: TransformField[Hd])
      => (tl: => TransformTuple[Tl]) => TransformTuple[Hd *: Tl] =
      new TransformTuple[Hd *: Tl]:
        def transformTuple(t: Tuple): Tuple =
          val net = t.asInstanceOf[NonEmptyTuple]
          hd.transformField(net.head.asInstanceOf[Hd]) *: tl.transformTuple(
            net.tail,
          )
  end TransformTuple

  abstract class SumCaseDispatch[S <: Sum, Cases <: Tuple]:
    def dispatch(t: Any): Any
  end SumCaseDispatch

  object SumCaseDispatch:
    given empty: [S <: Sum] => SumCaseDispatch[S, EmptyTuple] =
      new SumCaseDispatch[S, EmptyTuple]:
        def dispatch(t: Any): Any = throw MatchError(t)

    given cons: [S <: Sum, Hd <: Node, Tl <: Tuple] => (tag: NodeClassTag[Hd])
      => (xform: => Transform[Hd, ?]) => (rest: => SumCaseDispatch[S, Tl])
      => SumCaseDispatch[S, Hd *: Tl] =
      new SumCaseDispatch[S, Hd *: Tl]:
        def dispatch(t: Any): Any =
          if tag.isInstance(t) then xform.transformAny(t)
          else rest.dispatch(t)
  end SumCaseDispatch

  abstract class Transform[From <: Node, To <: Node]:
    private[Lang] def transformAny(t: Any): Any
  end Transform

  object Transform:
    extension [From <: Node, To <: Node](self: Transform[From, To])
      inline def transform[FT, TT](using
          NodeT.Aux[From, FT],
          NodeT.Aux[To, TT],
      )(t: FT): TT =
        self.transformAny(t).asInstanceOf[TT]

    given forTerm: [N <: Term[?], To <: Node] => (N: N)
      => (tm: TermMembers[N])
      => (tt: TransformTuple[NamedTuple.DropNames[tm.Members]])
      => (nt: NodeT[N]) => (rw: OptionalRewrite[nt.T])
      => Transform[N, To] =
      new Transform[N, To]:
        private[Lang] def transformAny(t: Any): Any =
          val raw = t.asInstanceOf[N.T]
          val transformed = tt.transformTuple(raw.members)
          val result = new N.T(transformed)
          rw.applyIfPresent(result)

    given forSum: [S <: Sum, To <: Node] => (etl: Sum.EffectiveTypeList[S])
      => (dispatch: SumCaseDispatch[S, etl.Cases])
      => (rw: SumOptionalRewrite[S])
      => Transform[S, To] =
      new Transform[S, To]:
        private[Lang] def transformAny(t: Any): Any =
          rw.applyOr(t, () => dispatch.dispatch(t))
  end Transform
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
      case L2.Bar((s, opt)) =>
        println((s, opt))
    end match

    ping2.ex match
      case L2.Bar((s, opt)) =>
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
        object Case:
          object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
          object Bob extends Lang.Term[(k: Int, foo: Foo.T)], Case
        export Case.*
      end Ping
    end L1
    object L1 extends L1

    trait L2 extends Lang.Extend[L1]:
      export up.{Foo as _, Ping as _, *}
      object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]
      object Ping extends up.Ping.Extends:
        sealed trait Case extends Lang.Node
        object Case:
          export up.Ping.Case.*
          object NewCase
              extends Lang.Term[(s: String, opt: Option[up.Foo.T])],
                Case
        export Case.*
      end Ping

      given up.Foo.ReplaceWith[Bar.type]()
      given up.Ping.ReplaceWith[Ping.type]()
    end L2
    object L2 extends L2

    // Error message should explain what doesn't match
    // summon[Conversion[L2.Bar.T, L2.Ping.T]]

    val nc: L2.Ping.T = L2.Ping.NewCase(s = "hello", opt = None)
    println(nc)

    val pong: L2.Ping.T =
      L2.Ping.Pong(k = 12, foo = L2.Bar(s = "x", opt = None))
    println(pong)

    nc.ex match
      case L2.Ping.NewCase((s, opt)) => println(s"NewCase: $s")
      case L2.Ping.Pong((k, foo))    => println(s"Pong: $k")
      case L2.Ping.Bob((k, foo))     => println(s"Bob: $k")
    end match

    pong.ex match
      case L2.Ping.NewCase((s, opt)) => println(s"NewCase: $s")
      case L2.Ping.Pong((k, foo))    => println(s"Pong: $k")
      case L2.Ping.Bob((k, foo))     => println(s"Bob: $k")
    end match
  end innerSumTest
end Test
