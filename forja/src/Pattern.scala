package forja

import cats.data.Chain
import scala.collection.mutable

import scala.compiletime.summonFrom

import Pattern.*

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
      fn: Context.ValueContext ?=> T => Node | Iterable[Node] | syntax.skipRewrite.type,
  ): Pattern[Unit] =
    new rewrite(pattern, fn(using Context.ValueContext))

  final def filter(pred: T => Boolean): Pattern[T] = new filter(pattern, pred)

  final def here[U](using ev: T <:< Node)(query: Query[U]): Pattern[U] =
    new here(pattern.map(ev), query)

  def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, Node.NodeSpan)]
end Pattern

object Pattern:
  enum MatchDir:
    case Left, Right
  end MatchDir
  final class Include[+T](val pattern: Pattern[T])

  private[forja] final class filter[T](
      val pattern: Pattern[T],
      pred: T => Boolean,
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      pattern.runPattern(nodeSpan, dir)
        .filter: (value, _) =>
          pred(value)
    end runPattern
  end filter

  private[forja] final class captureNode[T](val pattern: Pattern[T])
      extends Pattern[(Node, T)]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[((Node, T), NodeSpan)] =
      val nodeOpt = dir match
        case MatchDir.Right => nodeSpan.expandRightOption(1).map(_.last)
        case MatchDir.Left => nodeSpan.expandLeftOption(1).map(_.head)
      nodeOpt match
        case None       => None
        case Some(node) =>
          pattern.runPattern(nodeSpan, dir)
            .map: (value, nodeSpan) =>
              ((node, value), nodeSpan)
    end runPattern
  end captureNode

  private[forja] final class Tupled(
      val elems: Node.PatternApplyArg[Any]*,
  ) extends Pattern[Tuple]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(Tuple, NodeSpan)] =
      ???
      // def impl(
      //     i: Int,
      //     nodeSpan: NodeSpan,
      //     acc: Chain[Any],
      // ): Eval[Option[(Tuple, NodeSpan)]] =
      //   if i == elems.length
      //   then
      //     Eval.now(
      //       Some((Tuple.fromArray(acc.iterator.toArray), nodeSpan)),
      //     )
      //   else
      //     elems(i) match
      //       case include: Include[?] =>
      //         Eval
      //           .defer(include.pattern.runPattern(nodeSpan))
      //           .flatMap:
      //             case None                    => Eval.now(None)
      //             case Some((value, nodeSpan)) =>
      //               impl(i + 1, nodeSpan, acc :+ value)
      //       case pattern: Pattern[?] =>
      //         Eval
      //           .defer(pattern.runPattern(nodeSpan))
      //           .flatMap:
      //             case None                => Eval.now(None)
      //             case Some((_, nodeSpan)) =>
      //               impl(i + 1, nodeSpan, acc)
      //       case (
      //             token: Token,
      //             patternOrInclude: (Pattern[?] | Pattern.Include[?]),
      //           ) =>
      //         nodeSpan.parentOption match
      //           case None         => Eval.now(None)
      //           case Some(parent) =>
      //             parent.attrs.get(token) match
      //               case None        => Eval.now(None)
      //               case Some(child) =>
      //                 patternOrInclude match
      //                   case pattern: Pattern[?] =>
      //                     Eval
      //                       .defer(pattern.runPattern(child.emptyNodeSpanHere))
      //                       .flatMap:
      //                         case None    => Eval.now(None)
      //                         case Some(_) =>
      //                           impl(i + 1, nodeSpan, acc)
      //                   case include: Pattern.Include[?] =>
      //                     Eval
      //                       .defer(
      //                         include.pattern
      //                           .runPattern(child.emptyNodeSpanHere),
      //                       )
      //                       .flatMap:
      //                         case None             => Eval.now(None)
      //                         case Some((value, _)) =>
      //                           impl(i + 1, nodeSpan, acc :+ value)
      // end impl

      // impl(0, nodeSpan, Chain.empty)
    end runPattern
  end Tupled

  private[forja] final class alt[T](val left: Pattern[T], val right: Pattern[T])
      extends Pattern[T]:
    private lazy val decisionTree = Query.DecisionTree.fromPattern(this)
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      // decisionTree.run(nodeSpan, _.runPattern(nodeSpan, left))
      left.runPattern(nodeSpan, dir).orElse(right.runPattern(nodeSpan, dir))
    end runPattern
  end alt

  private[forja] final class map[T, U](val pattern: Pattern[T], val fn: T => U)
      extends Pattern[U]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(U, NodeSpan)] =
      pattern.runPattern(nodeSpan, dir)
        .map: (value, nodeSpan) =>
          (fn(value), nodeSpan)
    end runPattern
  end map

  private[forja] final class tokenExact[T](
      val token: Token,
      val pattern: Pattern[T],
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      dir match
        case MatchDir.Left =>
          nodeSpan.expandLeftOption(1) match
            case Some(nodeSoan) if nodeSpan.head.tokenOption.contains(token) =>
              pattern
                .runPattern(nodeSpan.head.children.asEmptyNodeSpan, MatchDir.Right)
                .map:
                  case (value, _) => (value, nodeSpan)
            case None | Some(_) => None
        case MatchDir.Right =>
          nodeSpan.expandRightOption(1) match
            case Some(nodeSpan) if nodeSpan.last.tokenOption.contains(token) =>
              pattern
                .runPattern(nodeSpan.last.children.asEmptyNodeSpan, MatchDir.Right)
                .map:
                  case (value, _) => (value, nodeSpan)
            case None | Some(_) => None
    end runPattern
  end tokenExact

  private[forja] final class tokenAny[T](val pattern: Pattern[T])
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      dir match
        case MatchDir.Left =>
          nodeSpan.expandLeftOption(1) match
            case None           => None
            case Some(nodeSpan) =>
              pattern
                .runPattern(nodeSpan.head.children.asEmptyNodeSpan, MatchDir.Right)
                .map:
                  case (value, _) => (value, nodeSpan)
        case MatchDir.Right =>
          nodeSpan.expandRightOption(1) match
            case None           => None
            case Some(nodeSpan) =>
              pattern
                .runPattern(nodeSpan.last.children.asEmptyNodeSpan, MatchDir.Right)
                .map:
                  case (value, _) => (value, nodeSpan)
    end runPattern
  end tokenAny

  private[forja] final class not[T](val pattern: Pattern[T])
      extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(Unit, NodeSpan)] =
      pattern.runPattern(nodeSpan, dir) match
        case None    => Some(((), nodeSpan))
        case Some(_) => None
    end runPattern
  end not

  private[forja] final class rewrite[T](
      val pattern: Pattern[T],
      val fn: T => Node | Iterable[Node] | syntax.skipRewrite.type,
  ) extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(Unit, NodeSpan)] =
      pattern.runPattern(nodeSpan, dir).map: (value, nodeSpan) =>
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
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(List[T], NodeSpan)] =
      var shouldContinue = true
      var nodeSpanAcc = nodeSpan
      val resultAcc = mutable.ListBuffer[T]()

      while shouldContinue
      do
        elem.runPattern(nodeSpanAcc, dir) match
          case None =>
            shouldContinue = false
          case Some((elem, nodeSpanAcc2)) =>
            resultAcc += elem
            nodeSpanAcc = nodeSpanAcc
      end while
      
      dir match
        case MatchDir.Left =>
          return Some((resultAcc.reverseIterator.toList, nodeSpanAcc))
        case MatchDir.Right =>
          return Some((resultAcc.result(), nodeSpanAcc))
    end runPattern
  end rep

  private[forja] final class embed[T: Node.Embed] extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      dir match
        case MatchDir.Left =>
          nodeSpan.expandLeftOption(1).flatMap: nodeSpan =>
            nodeSpan.last.valueOption.map((_, nodeSpan))
        case MatchDir.Right =>
          nodeSpan.expandRightOption(1).flatMap: nodeSpan =>
            nodeSpan.last.valueOption.map((_, nodeSpan))
    end runPattern
  end embed

  private[forja] final class EmbedLiteralPattern[T: Node.Embed](value: T)
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      dir match
        case MatchDir.Left =>
          nodeSpan.expandLeftOption(1).flatMap: nodeSpan =>
            nodeSpan.last.valueOption[T] match
              case Some(`value`) =>
                Some((value, nodeSpan))
              case None | Some(_) => None
        case MatchDir.Right =>
          nodeSpan.expandRightOption(1).flatMap: nodeSpan =>
            nodeSpan.last.valueOption[T] match
              case Some(`value`) =>
                Some((value, nodeSpan))
              case None | Some(_) => None
    end runPattern
  end EmbedLiteralPattern

  private[forja] final class here[T](pattern: Pattern[Node], query: Query[T]) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan, dir: MatchDir): Option[(T, NodeSpan)] =
      pattern.runPattern(nodeSpan, dir).flatMap: (node, nodeSpan) =>
        query.runQuery(node).value.map((_, nodeSpan))
    end runPattern
  end here
end Pattern
