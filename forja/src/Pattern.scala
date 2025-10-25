package forja

import cats.Eval
import cats.data.Chain

import scala.compiletime.summonFrom

import Pattern.*

type PatternContext = PatternContext.type
object PatternContext

sealed abstract class Pattern[+T]:
  pattern =>
  private[forja] def altOptions: Chain[Pattern[T]] =
    this match
      case pattern: alt[t] =>
        pattern.left.altOptions ++ pattern.right.altOptions
      case pattern => Chain.one(pattern)
  end altOptions

  final def unary_+ : Include[T] = Include(pattern)

  final transparent inline def unary_! : Include[Node | Tuple] =
    summonFrom:
      case given (T <:< Unit) =>
        Include(Pattern.captureNode(pattern).map(_._1))
      case ev: (T <:< (t & Tuple)) =>
        Include(Pattern.captureNode(pattern).map(p => p._1 *: ev(p._2)))
  end unary_!

  final def |[U >: T](other: Pattern[U]) = new alt(pattern, other)

  final def map[U](fn: T => U): Pattern[U] = new map(pattern, fn)

  final def rewrite(
      fn: T => Node | Iterable[Node] | syntax.skipRewrite.type,
  ): Pattern[Unit] =
    new rewrite(pattern, fn)

  final def filter(pred: T => Boolean): Pattern[T] = new filter(pattern, pred)

  def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, Node.NodeSpan)]]
end Pattern

object Pattern:
  final class Include[+T](val pattern: Pattern[T])

  private[forja] final class filter[T](
      val pattern: Pattern[T],
      pred: T => Boolean,
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      Eval
        .defer(pattern.runPattern(nodeSpan))
        .map:
          case None                    => None
          case some @ Some((value, _)) =>
            if pred(value)
            then some
            else None
    end runPattern
  end filter

  private[forja] final class captureNode[T](val pattern: Pattern[T])
      extends Pattern[(Node, T)]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[((Node, T), NodeSpan)]] =
      nodeSpan.expandRightOption(1).map(_.last) match
        case None       => Eval.now(None)
        case Some(node) =>
          Eval
            .defer(pattern.runPattern(nodeSpan))
            .map:
              case None                    => None
              case Some((value, nodeSpan)) => Some(((node, value), nodeSpan))
    end runPattern
  end captureNode

  private[forja] final class Tupled(
      val elems: Node.PatternApplyArg[Any]*,
  ) extends Pattern[Tuple]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Tuple, NodeSpan)]] =
      def impl(
          i: Int,
          nodeSpan: NodeSpan,
          acc: Chain[Any],
      ): Eval[Option[(Tuple, NodeSpan)]] =
        if i == elems.length
        then
          Eval.now(
            Some((Tuple.fromArray(acc.iterator.toArray), nodeSpan)),
          )
        else
          elems(i) match
            case include: Include[?] =>
              Eval
                .defer(include.pattern.runPattern(nodeSpan))
                .flatMap:
                  case None                    => Eval.now(None)
                  case Some((value, nodeSpan)) =>
                    impl(i + 1, nodeSpan, acc :+ value)
            case pattern: Pattern[?] =>
              Eval
                .defer(pattern.runPattern(nodeSpan))
                .flatMap:
                  case None                => Eval.now(None)
                  case Some((_, nodeSpan)) =>
                    impl(i + 1, nodeSpan, acc)
            case (
                  token: Token,
                  patternOrInclude: (Pattern[?] | Pattern.Include[?]),
                ) =>
              nodeSpan.parentOption match
                case None         => Eval.now(None)
                case Some(parent) =>
                  parent.attrs.get(token) match
                    case None        => Eval.now(None)
                    case Some(child) =>
                      patternOrInclude match
                        case pattern: Pattern[?] =>
                          Eval
                            .defer(pattern.runPattern(child.emptyNodeSpanHere))
                            .flatMap:
                              case None    => Eval.now(None)
                              case Some(_) =>
                                impl(i + 1, nodeSpan, acc)
                        case include: Pattern.Include[?] =>
                          Eval
                            .defer(
                              include.pattern
                                .runPattern(child.emptyNodeSpanHere),
                            )
                            .flatMap:
                              case None             => Eval.now(None)
                              case Some((value, _)) =>
                                impl(i + 1, nodeSpan, acc :+ value)
      end impl

      impl(0, nodeSpan, Chain.empty)
    end runPattern
  end Tupled

  private[forja] final class alt[T](val left: Pattern[T], val right: Pattern[T])
      extends Pattern[T]:
    private lazy val decisionTree = Query.DecisionTree.fromPattern(this)
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      decisionTree.run(nodeSpan, _.runPattern(nodeSpan))
      // Eval.defer(left.runPattern(nodeSpan)).flatMap:
      //   case None => right.runPattern(nodeSpan)
      //   case some: Some[(T, NodeSpan)] => Eval.now(some)
    end runPattern
  end alt

  private[forja] final class map[T, U](val pattern: Pattern[T], val fn: T => U)
      extends Pattern[U]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(U, NodeSpan)]] =
      Eval
        .defer(pattern.runPattern(nodeSpan))
        .map:
          _.map: (value, nodeSpan) =>
            (fn(value), nodeSpan)
    end runPattern
  end map

  private[forja] final class tokenExact[T](
      val token: Token,
      val pattern: Pattern[T],
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case Some(nodeSpan) if nodeSpan.last.tokenOption.contains(token) =>
          pattern
            .runPattern(nodeSpan.last.children.asEmptyNodeSpan)
            .map:
              _.map:
                case (value, _) => (value, nodeSpan)
        case None | Some(_) => Eval.now(None)
    end runPattern
  end tokenExact

  private[forja] final class tokenAny[T](val pattern: Pattern[T])
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None           => Eval.now(None)
        case Some(nodeSpan) =>
          pattern
            .runPattern(nodeSpan.last.children.asEmptyNodeSpan)
            .map:
              _.map:
                case (value, _) => (value, nodeSpan)
    end runPattern
  end tokenAny

  private[forja] final class not[T](val pattern: Pattern[T])
      extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Unit, NodeSpan)]] =
      Eval
        .defer(pattern.runPattern(nodeSpan))
        .map:
          case None    => Some(((), nodeSpan))
          case Some(_) => None
    end runPattern
  end not

  private[forja] object endOfSpan extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Unit, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None    => Eval.now(Some(((), nodeSpan)))
        case Some(_) => Eval.now(None)
    end runPattern
  end endOfSpan

  private[forja] final class rewrite[T](
      val pattern: Pattern[T],
      val fn: T => Node | Iterable[Node] | syntax.skipRewrite.type,
  ) extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Unit, NodeSpan)]] =
      Eval
        .defer(pattern.runPattern(nodeSpan))
        .map:
          _.map: (value, nodeSpan) =>
            val result = fn(value) match
              case node: Node =>
                ((), nodeSpan.replaceThis(List(node)))
              case nodes: Iterable[Node] =>
                ((), nodeSpan.replaceThis(nodes))
              case syntax.skipRewrite =>
                ((), nodeSpan)

            // println(s"rewrite ${nodeSpan.parentOption}--> ${result._2.parentOption}")
            result
    end runPattern
  end rewrite

  private[forja] final class rep[T](val elem: Pattern[T])
      extends Pattern[List[T]]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(List[T], NodeSpan)]] =
      def impl(
          nodeSpan: NodeSpan,
          acc: Chain[T],
      ): Eval[Option[(List[T], NodeSpan)]] =
        Eval
          .defer(elem.runPattern(nodeSpan))
          .flatMap:
            case None => Eval.now(Some((acc.toList, nodeSpan)))
            case Some((elem, nodeSpan)) =>
              impl(nodeSpan, acc :+ elem)
      end impl

      impl(nodeSpan, Chain.empty)
    end runPattern
  end rep

  private[forja] final class embed[T: Node.Embed] extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None           => Eval.now(None)
        case Some(nodeSpan) =>
          Eval.now(nodeSpan.last.valueOption.map((_, nodeSpan)))
    end runPattern
  end embed

  private[forja] final class EmbedLiteralPattern[T: Node.Embed](value: T)
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None           => Eval.now(None)
        case Some(nodeSpan) =>
          nodeSpan.last.valueOption[T] match
            case Some(`value`) =>
              Eval.now(Some((value, nodeSpan)))
            case None | Some(_) => Eval.now(None)
    end runPattern
  end EmbedLiteralPattern

  final class queryPrev[T](query: Query[T]) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.lastOption match
        case None       => Eval.now(None)
        case Some(node) =>
          query
            .runQuery(node)
            .map:
              case None        => None
              case Some(value) => Some((value, nodeSpan))
    end runPattern
  end queryPrev
end Pattern
