package forja

import scala.quoted.Type
import scala.annotation.publicInBinary
import forja.util.TrivialMatch
import scala.compiletime.erasedValue
import scala.compiletime.Erased

trait Lang2:

end Lang2

object Lang2:
  sealed trait IsNode[N] extends Erased:
    type L <: Lang2
    type Path <: Tuple
  end IsNode

  object IsNode:
    final class Aux[N, L0 <: Lang2, Path0 <: Tuple] extends IsNode[N]:
      type L = L0
      type Path = Path0
    end Aux

    transparent inline given instance: [N] => (inline ev: Term[N] | Sum[N]) => Aux[N, ?, ?] =
      ${ MetaMacros.isNodeImpl[N] }
    end instance
  end IsNode

  sealed trait LaunderNode[L2 <: Lang2, N1]:
    type N2
  end LaunderNode

  object LaunderNode:
    final class Aux[L2 <: Lang2, N1, N20] extends LaunderNode[L2, N1]:
      type N2 = N20
    end Aux

    transparent inline given instance: [L2 <: Lang2, N1] => (isn: IsNode[N1]) => Aux[L2, N1, ?] =
      ${ MetaMacros.launderNodeImpl[isn.Path, L2, N1] }
    end instance
  end LaunderNode

  final class HasSameMeta[N1, N2] extends Erased

  object HasSameMeta:
    transparent inline given instance: [N1, N2] => (i1: IsNode[N1]) => (i2: IsNode[N2]) => (launder: LaunderNode[i2.L, N1]) => (inline ev: launder.N2 =:= N2) => HasSameMeta[N1, N2] =
      ${ MetaMacros.hasSameMetaImpl[N1, N2, i1.L, i2.L] }
    end instance
  end HasSameMeta

  sealed trait NodeMeta[T]:
    type Companion <: Singleton
    def Companion: Companion
    
    type Cases <: Tuple
  end NodeMeta

  type UnapplyResult[Cases <: Tuple] = Cases match
    case EmptyTuple => true
    case Tuple1[t] => TrivialMatch[t]
    case _ => TrivialMatch[Cases]
  end UnapplyResult

  trait TermMeta[T] extends NodeMeta[T]:
    type Labels <: Tuple

    extension (Companion: Companion)
      inline def apply(using ev: EmptyTuple =:= Cases)(): T =
        applyImpl(ev(EmptyTuple))
      end apply

      inline def apply[E](using ev: Tuple1[E] =:= Cases)(e: E): T =
        applyImpl(ev(Tuple1(e)))
      end apply

      inline def apply(cases: NamedTuple.NamedTuple[Labels, Cases]): T =
        applyImpl(cases)
      end apply

      inline def unapply(t: T): UnapplyResult[Cases] =
        inline erasedValue[Cases] match
          case _: EmptyTuple => true
          case _: Tuple1[t] => ???
          case _ => ???
        end match
      end unapply
    end extension

    @publicInBinary
    private[Lang2] def applyImpl(cases: Cases): T
  end TermMeta

  trait SumMeta[T] extends NodeMeta[T]:
    extension (Companion: Companion)
      def ordinal(t: T): Int

      inline def apply(inline elem: Tuple.Union[Cases]): T =
        ${ MetaMacros.sumApplyImpl[Cases, T]('{ this }, '{ elem }) }
      end apply
    end extension

    @publicInBinary
    private[forja] def applyImpl(idx: Int, elem: ErasedNode): T
  end SumMeta

  trait Impl[L <: Lang2]:
    transparent inline given termMeta: [T] => (isn: IsNode[T]) => (inline ev: isn.L <:< L) => (term: Term[T]) => TermMeta[T] =
      ${ MetaMacros.termMetaImpl[L, T]('{ term }) }
    end termMeta

    transparent inline given sumMeta: [T] => (isn: IsNode[T]) => (inline ev: isn.L <:< L) => (sum: Sum[T]) => SumMeta[T] =
      ${ MetaMacros.sumMetaImpl[L, T]('{ sum }) }
    end sumMeta
  end Impl

  trait Ext:
    L: Lang2 =>

  end Ext

  private[forja] trait ErasedNode:

  end ErasedNode

  sealed trait EffectiveType[U] extends Erased:
    type T
  end EffectiveType

  object EffectiveType:

  end EffectiveType
end Lang2

