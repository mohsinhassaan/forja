package forja

import scala.collection.concurrent

final class Token private (val fullName: String):
  inline def applyTupled[Tp <: Tuple, U](tp: Tp)(using
      ctx: syntax.Context,
  )(using app: ctx.NodeApply[Token *: Tp, U]): U = app(this *: tp)
  
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
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21, t22: T22)(using ctx: syntax.Context)(using app: ctx.NodeApply[(Token, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), U]): U = app((this, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22))
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
