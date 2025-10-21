package forja

import Pattern.*
import cats.data.Chain
import cats.Eval

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

  final def |[U >: T](other: Pattern[U]) = new alt(pattern, other)

  final def map[U](fn: T => U): Pattern[U] = new map(pattern, fn)

  final def rewrite(fn: T => Node | Iterable[Node]): Pattern[Unit] = new rewrite(pattern, fn)

  def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, Node.NodeSpan)]]
end Pattern

object Pattern:
  final class Include[+T](val pattern: Pattern[T])

  private[forja] final class Tupled[Tuple](val elems: (Pattern[?] | Include[?])*) extends Pattern[Tuple]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Tuple, NodeSpan)]] =
      def impl(i: Int, nodeSpan: NodeSpan, acc: Chain[Any]): Eval[Option[(Tuple, NodeSpan)]] =
        if i == elems.length
        then
          Eval.now(Some((Tuple.fromArray(acc.iterator.toArray).asInstanceOf, nodeSpan)))
        else
          elems(i) match
            case include: Include[?] =>
              Eval.defer(include.pattern.runPattern(nodeSpan)).flatMap:
                case None => Eval.now(None)
                case Some((value, nodeSpan)) =>
                  impl(i + 1, nodeSpan, acc :+ value)
            case pattern: Pattern[?] =>
              Eval.defer(pattern.runPattern(nodeSpan)).flatMap:
                case None => Eval.now(None)
                case Some((_, nodeSpan)) =>
                  impl(i + 1, nodeSpan, acc)
      end impl
      
      impl(0, nodeSpan, Chain.empty)
    end runPattern
  end Tupled

  private[forja] final class alt[T](val left: Pattern[T], val right: Pattern[T]) extends Pattern[T]:
    private lazy val decisionTree = Query.DecisionTree.fromPattern(this)
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      decisionTree.run(nodeSpan, _.runPattern(nodeSpan))
      // Eval.defer(left.runPattern(nodeSpan)).flatMap:
      //   case None => right.runPattern(nodeSpan)
      //   case some: Some[(T, NodeSpan)] => Eval.now(some)
    end runPattern
  end alt

  private[forja] final class map[T, U](val pattern: Pattern[T], val fn: T => U) extends Pattern[U]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(U, NodeSpan)]] =
      Eval.defer(pattern.runPattern(nodeSpan)).map:
        _.map: (value, nodeSpan) =>
          (fn(value), nodeSpan)
    end runPattern
  end map

  private[forja] final class tokenExact[T](val token: Token, val pattern: Pattern[T]) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case Some(nodeSpan) if nodeSpan.last.tokenOption.contains(token) =>
          pattern.runPattern(nodeSpan.last.children.asEmptyNodeSpan).map:
            _.map:
              case (value, _) => (value, nodeSpan)
        case None | Some(_) => Eval.now(None)
    end runPattern
  end tokenExact

  private[forja] final class tokenAny[T](val pattern: Pattern[T]) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(T, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None => Eval.now(None)
        case Some(nodeSpan) =>
          pattern.runPattern(nodeSpan.last.children.asEmptyNodeSpan).map:
            _.map:
              case (value, _) => (value, nodeSpan)
    end runPattern
  end tokenAny

  private[forja] object endOfSpan extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Unit, NodeSpan)]] =
      nodeSpan.expandRightOption(1) match
        case None => Eval.now(Some(((), nodeSpan)))
        case Some(_) => Eval.now(None)
    end runPattern
  end endOfSpan

  private[forja] final class rewrite[T](val pattern: Pattern[T], val fn: T => Node | Iterable[Node]) extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(Unit, NodeSpan)]] =
      Eval.defer(pattern.runPattern(nodeSpan)).map:
        _.map: (value, nodeSpan) =>
          fn(value) match
            case node: Node =>
              ((), nodeSpan.replaceThis(List(node)))
            case nodes: Iterable[Node] =>
              ((), nodeSpan.replaceThis(nodes))
    end runPattern
  end rewrite

  final class rep[T](val elem: Pattern[T]) extends Pattern[List[T]]:
    def runPattern(nodeSpan: NodeSpan): Eval[Option[(List[T], NodeSpan)]] = 
      def impl(nodeSpan: NodeSpan, acc: Chain[T]): Eval[Option[(List[T], NodeSpan)]] =
        Eval.defer(elem.runPattern(nodeSpan)).flatMap:
          case None => Eval.now(Some((acc.toList, nodeSpan)))
          case Some((elem, nodeSpan)) =>
            impl(nodeSpan, acc :+ elem)
      end impl

      impl(nodeSpan, Chain.empty)
    end runPattern
  end rep

  def rep1[T](elem: Pattern[T])(using PatternContext): Pattern[List[T]] =
    NodeSpan(+elem, +rep(elem)).map(_ :: _)
  end rep1

  // TODO: attr pattern
end Pattern
