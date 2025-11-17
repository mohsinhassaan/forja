package forja

import cats.data.Chain
import cats.{Alternative, Eval, Foldable}

import forja.util.ReflectiveEnumeration

import scala.collection.mutable
import scala.reflect.TypeTest

import Query.*

sealed trait Query[+T]:
  query =>
  private[Query] final def altOptions: Chain[Query[T]] =
    query match
      case query: alt[u] =>
        query.left.altOptions ++ query.right.altOptions
      case _ => Chain.one(query)
  end altOptions

  final infix def |[U >: T](other: Query[U]): Query[U] = new alt(query, other)

  final def map[U](fn: T => U): Query[U] = new map(query, fn)

  final def cast[TT >: T, U](using U <:< TT): Query[U] = query.asInstanceOf

  final def here[TT >: T, U](at: Query[U])(using ev: TT <:< Node): Query[U] =
    new here(query.cast(using ev), at)

  final def runQuery(node: Node): Eval[Option[T]] =
    runQueryImpl(node)

  protected def runQueryImpl(node: Node): Eval[Option[T]]
end Query

object Query:
  given alternative: Alternative[Query]:
    def ap[A, B](ff: Query[A => B])(fa: Query[A]): Query[B] = new ap(ff, fa)
    def combineK[A](x: Query[A], y: Query[A]): Query[A] = x | y
    def empty[A]: Query[A] = Query.empty
    def pure[A](x: A): Query[A] = Query.pure(x)
  end alternative

  object empty extends Query[Nothing]:
    protected def runQueryImpl(node: Node): Eval[Option[Nothing]] =
      Eval.now(None)
    end runQueryImpl
  end empty

  final class pure[T](value: T) extends Query[T]:
    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      Eval.now(Some(value))
    end runQueryImpl
  end pure

  final class on[+T](val pattern: Pattern[T]) extends Query[T]:
    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      Eval.now(pattern.runPattern(node.emptyNodeSpanHere).map(_._1))
    end runQueryImpl

    def rewrite[U >: T](
        fn: syntax.ValueContext.type ?=> U => Node | Iterable[Node] |
          syntax.unchanged.type,
    ): rewrite[U] =
      new rewrite(pattern, fn)
    end rewrite
  end on

  object on:
    private type C = syntax.PatternContext.type
    private given C = syntax.PatternContext

    inline def applyTupled[Tp <: Tuple, U](arg: C ?=> Tp)(using
        app: syntax.PatternContext.NodeSpanApply[Tp, Pattern[U]],
    ): on[U] =
      new on[U](app(arg))

    // format: off
    inline def apply[U]()(using app: syntax.PatternContext.NodeSpanApply[EmptyTuple, Pattern[U]]): on[U] = new on[U](app(EmptyTuple))
    inline def apply[T1, U](t1: C ?=> T1)(using app: syntax.PatternContext.NodeSpanApply[Tuple1[T1], Pattern[U]]): on[U] = new on[U](app(Tuple1(t1)))
    // %%replicate22
    inline def apply[T1, T2, U](t1: C ?=> T1, t2: C ?=> T2)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2), Pattern[U]]): on[U] = new on[U](app((t1, t2)))
    inline def apply[T1, T2, T3, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3)))
    inline def apply[T1, T2, T3, T4, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4)))
    inline def apply[T1, T2, T3, T4, T5, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5)))
    inline def apply[T1, T2, T3, T4, T5, T6, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17, t18: C ?=> T18)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17, t18: C ?=> T18, t19: C ?=> T19)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17, t18: C ?=> T18, t19: C ?=> T19, t20: C ?=> T20)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17, t18: C ?=> T18, t19: C ?=> T19, t20: C ?=> T20, t21: C ?=> T21)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, U](t1: C ?=> T1, t2: C ?=> T2, t3: C ?=> T3, t4: C ?=> T4, t5: C ?=> T5, t6: C ?=> T6, t7: C ?=> T7, t8: C ?=> T8, t9: C ?=> T9, t10: C ?=> T10, t11: C ?=> T11, t12: C ?=> T12, t13: C ?=> T13, t14: C ?=> T14, t15: C ?=> T15, t16: C ?=> T16, t17: C ?=> T17, t18: C ?=> T18, t19: C ?=> T19, t20: C ?=> T20, t21: C ?=> T21, t22: C ?=> T22)(using app: syntax.PatternContext.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), Pattern[U]]): on[U] = new on[U](app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)))
    // format: on
  end on

  final class rewrite[T](
      srcPattern: Pattern[T],
      fn: syntax.ValueContext.type ?=> T => Node | Iterable[Node] |
        syntax.unchanged.type,
  ) extends ReflectiveEnumeration.Enumerable:
    val pattern = srcPattern.rewrite(fn)
  end rewrite

  private final class map[T, U](val query: Query[T], fn: T => U)
      extends Query[U]:
    protected def runQueryImpl(node: Node): Eval[Option[U]] =
      Eval.defer(query.runQuery(node)).map(_.map(fn))
    end runQueryImpl
  end map

  private final class ap[A, B](val ff: Query[A => B], val fa: Query[A])
      extends Query[B]:
    protected def runQueryImpl(node: Node): Eval[Option[B]] =
      Eval
        .defer(ff.runQuery(node))
        .flatMap:
          case None     => Eval.now(None)
          case Some(ff) =>
            Eval.defer(fa.runQuery(node)).map(_.map(ff))
    end runQueryImpl
  end ap

  private final class here[T](val place: Query[Node], val query: Query[T])
      extends Query[T]:
    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      place
        .runQuery(node)
        .flatMap:
          case Some(node) =>
            query.runQuery(node)
          case None => Eval.now(None)
    end runQueryImpl
  end here

  private final class alt[T](val left: Query[T], val right: Query[T])
      extends Query[T]:
    import DecisionTree.*
    private lazy val decisionTree: FrozenRecordList[T, Query] =
      DecisionTree.fromQuery(this)

    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      decisionTree.run(node.emptyNodeSpanHere, _.runQuery(node))
    end runQueryImpl
  end alt

  private[forja] object DecisionTree:
    private def scanPattern[T, Q[_] <: Query[?] | Pattern[?]](
        buf: RecordList[T, Q],
        pattern: Pattern[?],
    ): Chain[Either[RecordList[T, Q], RecordList[T, Q]]] =
      pattern match
        case pattern: Pattern.Tupled[?] =>
          ???
          // pattern.elems.foldLeft(
          //   Chain.one(Right(buf): Either[RecordList[T, Q], RecordList[T, Q]]),
          // ): (acc, elem) =>
          //   acc.flatMap:
          //     case Left(buf)  => Chain.one(Left(buf))
          //     case Right(buf) =>
          //       elem match
          //         case pattern: Pattern[?] =>
          //           scanPattern(buf, pattern)
          //         case include: Pattern.Include[?] =>
          //           scanPattern(buf, include.pattern)
          //         case (_: Token, _) =>
          //           // attrs have no impact on decision tree
          //           Chain.one(Right(buf))
        case pattern: Pattern.alt[?] =>
          scanPattern(buf, pattern.left)
            ++ scanPattern(buf, pattern.right)
        case pattern: Pattern.map[?, ?] =>
          scanPattern(buf, pattern.pattern)
        case pattern: Pattern.tokenExact[?] =>
          Chain.one(Right(buf.ensureBranch.upsert(pattern.token)))
        case _: (Pattern.tokenAny[?] | Pattern.rep[?] |
              Pattern.rewriteMap[?, ?]) =>
          Chain.one(Left(buf))
        case _: Pattern.embed[?] =>
          Chain.one(Left(buf))
        case pattern: Pattern.EmbedLiteralPattern[?] =>
          // TODO: add embeds to tree
          Chain.one(Left(buf))
        case pattern: Pattern.filter[?] =>
          scanPattern(buf, pattern.pattern)
        case pattern: Pattern.captureNode[?] =>
          scanPattern(buf, pattern.pattern)
        case _: Pattern.not[?] =>
          Chain.one(Right(buf))
        case pattern: Pattern.here[?] =>
          scanPattern(buf, pattern).map(_.forceLeft)
    end scanPattern

    private def scanQuery[T, Q[_] <: Query[?] | Pattern[?]](
        buf: RecordList[T, Q],
        query: Query[?],
    ): Chain[Either[RecordList[T, Q], RecordList[T, Q]]] =
      query match
        case Query.empty | (_: pure[?]) | Query.leftSibling |
            Query.rightSibling | Query.parent | (_: NodeAccessorQuery[?]) =>
          Chain.one(Right(buf))
        case query: on[?] =>
          scanPattern(buf, query.pattern)
        case query: map[?, ?] =>
          scanQuery(buf, query.query)
        case query: ap[?, ?] =>
          val left = scanQuery(buf, query.ff)
          val right = scanQuery(buf, query.fa)
          val leftMoved = left.exists(_.both ne buf)
          val rightMoved = right.exists(_.both ne buf)
          val hasLeft = left.exists(_.isLeft) || right.exists(_.isLeft)

          // If both sides "moved", then don't know what to do.
          // Exclude ourselves from decision tree at this point.
          if leftMoved && rightMoved
          then Chain.one(Left(buf))
          else if leftMoved
          then if hasLeft then left.map(_.forceLeft) else left
          else if hasLeft then right.map(_.forceLeft)
          else right
        case query: here[?] =>
          scanQuery(buf, query.place).map(_.forceLeft)
        case query: alt[?] =>
          scanQuery(buf, query.left)
            ++ scanQuery(buf, query.right)
        case Query.currentNode =>
          Chain.one(Left(buf))
    end scanQuery

    def fromQuery[T](query: Query[T]): FrozenRecordList[T, Query] =
      val acc = new RecordList[T, Query]
      query.altOptions.iterator.foreach: opt =>
        scanQuery(acc, opt).iterator
          .map(_.both)
          .foreach(_ += opt)
      acc.result().map(_.freeze)
    end fromQuery

    def fromPattern[T](pattern: Pattern[T]): FrozenRecordList[T, Pattern] =
      val acc = new RecordList[T, Pattern]
      pattern.altOptions.iterator.foreach: opt =>
        scanPattern(acc, opt).iterator
          .map(_.both)
          .foreach(_ += opt)
      acc.result().map(_.freeze)
    end fromPattern

    type RecordList[T, Q[_] <: Query[?] | Pattern[?]] =
      mutable.ListBuffer[Record[T, Q]]
    type FrozenRecordList[T, Q[_] <: Query[?] | Pattern[?]] =
      List[FrozenRecord[T, Q]]
    type Record[T, Q[_] <: Query[?] | Pattern[?]] = Q[T] | Branch[T, Q]
    final case class Branch[T, Q[_] <: Query[?] | Pattern[?]](
        map: mutable.HashMap[Token | EndOfFieldsMarker, RecordList[T, Q]],
    ):
      def upsert(key: Token | EndOfFieldsMarker): RecordList[T, Q] =
        map.getOrElseUpdate(key, mutable.ListBuffer.empty)
      end upsert
    end Branch

    extension [T, Q[_] <: Query[?] | Pattern[?]](recordList: RecordList[T, Q])
      def ensureBranch: Branch[T, Q] =
        recordList.lastOption
          .filter(_.isInstanceOf[Branch[T, Q]])
          .map(_.asInstanceOf[Branch[T, Q]])
          .getOrElse:
            val branch = new Branch[T, Q](
              mutable.HashMap.empty[Token | EndOfFieldsMarker, RecordList[T, Q]],
            )
            recordList += branch
            branch
      end ensureBranch
    end extension

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        either: Either[RecordList[T, Q], RecordList[T, Q]]
    )
      def forceLeft: Either[RecordList[T, Q], RecordList[T, Q]] =
        if either.isRight
        then either.swap
        else either
      end forceLeft

      def both: RecordList[T, Q] =
        either.fold(identity, identity)
      end both
    end extension

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        record: Record[T, Q]
    )(using TypeTest[Record[T, Q], Q[T]])
      def freeze: FrozenRecord[T, Q] =
        record match
          case Branch[T, Q](map) =>
            FrozenBranch(map.view.mapValues(_.map(_.freeze).result()).toMap)
          case q: Q[T] => q
      end freeze
    end extension

    type FrozenRecord[T, Q[_] <: Query[?] | Pattern[?]] = Q[T] |
      FrozenBranch[T, Q]
    final case class FrozenBranch[T, Q[_] <: Query[?] | Pattern[?]](
        map: Map[Token | EndOfFieldsMarker, FrozenRecordList[T, Q]],
    )

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        recs: FrozenRecordList[T, Q]
    )(using TypeTest[FrozenRecord[T, Q], Q[T]])
      def run[U](
          nodeSpan: NodeSpan,
          runFn: Q[T] => Eval[Option[U]],
      ): Eval[Option[U]] =
        def impl(
            nodeSpan: NodeSpan,
            recs: FrozenRecordList[T, Q],
        ): Eval[Option[U]] =
          Foldable[List].collectFirstSomeM(recs):
            case query: Q[T] =>
              runFn(query)
            case FrozenBranch[T, Q](map) =>
              nodeSpan.expandRightOption(1) match
                case None =>
                  map.get(EndOfFieldsMarker) match
                    case None       => Eval.now(None)
                    case Some(recs) =>
                      impl(nodeSpan, recs)
                case Some(nodeSpan) =>
                  nodeSpan.last.tokenOption match
                    case None        => Eval.now(None)
                    case Some(token) =>
                      map.get(token) match
                        case None        => Eval.now(None)
                        case Some(query) =>
                          impl(nodeSpan, recs)
        end impl
        impl(nodeSpan, recs)
      end run
    end extension

    type EndOfFieldsMarker = EndOfFieldsMarker.type
    object EndOfFieldsMarker
  end DecisionTree

  object currentNode extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(Some(node))
    end runQueryImpl
  end currentNode

  transparent trait NodeAccessorQuery[T](query: Query[T]) extends Query[T]:
    protected def access(node: Node): Option[Node]
    protected final def runQueryImpl(node: Node): Eval[Option[T]] =
      access(node) match
        case None       => Eval.now(None)
        case Some(node) => Eval.defer(query.runQuery(node))
    end runQueryImpl
  end NodeAccessorQuery

  final class leftSibling[T](query: Query[T])
      extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.leftSiblingOption
  end leftSibling

  object leftSibling extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.leftSiblingOption)
  end leftSibling

  final class rightSibling[T](query: Query[T])
      extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.rightSiblingOption
  end rightSibling

  object rightSibling extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.rightSiblingOption)
  end rightSibling

  final class parent[T](query: Query[T]) extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.parentOption
  end parent

  object parent extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.parentOption)
  end parent
end Query
