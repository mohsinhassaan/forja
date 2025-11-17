package forja

import scala.collection.concurrent

final class Token private (val fullName: String):
  inline def applyTupled[Tp <: Tuple, U](tp: Tp)(using ctx: syntax.Context)(using app: ctx.NodeApply[Token *: Tp, U]): U = app(this *: tp)
  
  // format: off
  inline def apply[U]()(using ctx: syntax.Context)(using app: ctx.NodeApply[Tuple1[Token], U]): U = app(Tuple1(this))
  inline def apply[T1, U](t1: T1)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1), U]): U = app((this, t1))
  // %%replicate22
  inline def apply[T1, T2, U](t1: T1, t2: T2)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2), U]): U = app((this, t1, t2))
  inline def apply[T1, T2, T3, U](t1: T1, t2: T2, t3: T3)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3), U]): U = app((this, t1, t2, t3))
  inline def apply[T1, T2, T3, T4, U](t1: T1, t2: T2, t3: T3, t4: T4)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4), U]): U = app((this, t1, t2, t3, t4))
  inline def apply[T1, T2, T3, T4, T5, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5), U]): U = app((this, t1, t2, t3, t4, t5))
  inline def apply[T1, T2, T3, T4, T5, T6, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6), U]): U = app((this, t1, t2, t3, t4, t5, t6))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8))
  // format: on

  override def toString(): String = fullName
end Token

object Token:
  private val byNameStore = concurrent.TrieMap[String, Token]()
  def byName(fullName: String): Token =
    byNameStore.getOrElseUpdate(fullName, new Token(fullName))
  end byName

  def apply(shapes: => Wf.Shapes*)(using
      fullName: sourcecode.FullName,
  ): Wf.TokenWf =
    Wf.TokenWf(byName(fullName.value), Wf.ShapeSeq(shapes*))
  end apply
end Token
