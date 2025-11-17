package forja

import cats.data.Chain

import scala.annotation.tailrec
import scala.collection.mutable

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

  final def unary_![U >: T <: Tuple](using ev: T <:< U): Include[Node *: U] =
    Include(Pattern.captureNode(pattern).map(p => p._1 *: ev(p._2)))
  end unary_!

  final def |[U >: T](other: Pattern[U]) = new alt(pattern, other)

  final def map[U](fn: T => U): Pattern[U] = new map(pattern, fn)

  final def rewrite(
      fn: syntax.ValueContext.type ?=> T => Node | Iterable[Node] |
        syntax.unchanged.type,
  ): Pattern[Unit] =
    rewriteMap: t =>
      ((), fn(t))
  end rewrite

  final def rewriteMap[U](
      fn: syntax.ValueContext.type ?=> T => (
          U,
          Node | Iterable[Node] | syntax.unchanged.type,
      ),
  ): Pattern[U] =
    new rewriteMap(pattern, fn(using syntax.ValueContext))
  end rewriteMap

  final def filter(pred: T => Boolean): Pattern[T] = new filter(pattern, pred)

  final def here[U](using ev: T <:< Node)(query: Query[U]): Pattern[U] =
    new here(pattern.map(ev), query)

  def runPattern(nodeSpan: NodeSpan): Option[(T, Node.NodeSpan)]
end Pattern

object Pattern:
  final class Include[+T](val pattern: Pattern[T])

  private[forja] final class filter[T](
      val pattern: Pattern[T],
      pred: T => Boolean,
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      pattern
        .runPattern(nodeSpan)
        .filter: (value, _) =>
          pred(value)
    end runPattern
  end filter

  private[forja] final class captureNode[T](val pattern: Pattern[T])
      extends Pattern[(Node, T)]:
    def runPattern(nodeSpan: NodeSpan): Option[((Node, T), NodeSpan)] =
      val nodeIdx = nodeSpan.size
      pattern
        .runPattern(nodeSpan)
        .flatMap: (value, nodeSpan) =>
          nodeSpan
            .lift(nodeIdx)
            .map: node =>
              ((node, value), nodeSpan)
    end runPattern
  end captureNode

  private[forja] final class Tupled[Result <: Tuple](
      tupleArity: Int,
      isTotal: Boolean,
      val cases: List[Tupled.Case],
  ) extends Pattern[Result]:
    def runPattern(nodeSpan: NodeSpan): Option[(Result, NodeSpan)] =
      import Tupled.*
      val resultsArr = Array.ofDim[Any](tupleArity)
      @tailrec
      def impl(
          cases: List[Tupled.Case],
          resultIdx: Int,
          nodeSpan: NodeSpan,
          searchStack: List[
            (resultIdx: Int, nodeSpan: NodeSpan, cases: List[Tupled.Case]),
          ],
      ): Option[(Result, NodeSpan)] =
        inline def failCase: Option[(Result, NodeSpan)] =
          searchStack match
            case Nil =>
              None
            case hd :: searchStackTl =>
              impl(hd.cases, hd.resultIdx, hd.nodeSpan, searchStackTl)
        end failCase

        cases match
          case Nil =>
            if isTotal && nodeSpan.expandRightMax.size == nodeSpan.size
            then
              Some((Tuple.fromArray(resultsArr).asInstanceOf[Result], nodeSpan))
            else if !isTotal
            then
              Some((Tuple.fromArray(resultsArr).asInstanceOf[Result], nodeSpan))
            else failCase
          case IncludeTupleCase(pattern) :: casesTl =>
            pattern.runPattern(nodeSpan) match
              case None                  => failCase
              case Some((tpl, nodeSpan)) =>
                (0 until tpl.size).foreach: i =>
                  resultsArr(resultIdx + i) = tpl.productElement(i)
                impl(
                  casesTl,
                  resultIdx = resultIdx + tpl.size,
                  nodeSpan = nodeSpan,
                  searchStack = searchStack,
                )
          case IncludeCase(pattern) :: casesTl =>
            pattern.runPattern(nodeSpan) match
              case None                   => failCase
              case Some((elem, nodeSpan)) =>
                resultsArr(resultIdx) = elem
                impl(
                  casesTl,
                  resultIdx = resultIdx + 1,
                  nodeSpan = nodeSpan,
                  searchStack = searchStack,
                )
          case SkipCase(pattern) :: casesTl =>
            pattern.runPattern(nodeSpan) match
              case None                => failCase
              case Some((_, nodeSpan)) =>
                impl(
                  casesTl,
                  resultIdx = resultIdx,
                  nodeSpan = nodeSpan,
                  searchStack = searchStack,
                )
          case WildcardCase :: casesTl =>
            // If there is a right-ward position to try, add a search stack record
            // where this wildcard evaluates there. If just ignoring this wildcard
            // makes the pattern fail, then we will retry one spot to the right,
            // including seeing this wildcard and adding another stack entry 1 further,
            // and so on. The only case we don't retry is if we are at end of seq,
            // and no right-ward step is possible.
            val nextSearchStack =
              nodeSpan.expandRightOption(1) match
                case None               => searchStack
                case Some(nextNodeSpan) =>
                  (
                    resultIdx = resultIdx,
                    nodeSpan = nextNodeSpan,
                    cases = cases,
                  ) :: searchStack
            end nextSearchStack
            impl(
              casesTl,
              resultIdx = resultIdx,
              nodeSpan = nodeSpan,
              searchStack = nextSearchStack,
            )
      end impl

      impl(cases, resultIdx = 0, nodeSpan = nodeSpan, searchStack = Nil)
    end runPattern
  end Tupled

  private[forja] object Tupled:
    sealed trait Case

    final case class IncludeTupleCase[T <: Tuple](pattern: Pattern[T])
        extends Case
    final case class IncludeCase[T](pattern: Pattern[T]) extends Case
    final case class SkipCase[T](pattern: Pattern[T]) extends Case
    object WildcardCase extends Case
  end Tupled

  private[forja] final class alt[T](val left: Pattern[T], val right: Pattern[T])
      extends Pattern[T]:
    private lazy val decisionTree = Query.DecisionTree.fromPattern(this)
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      // decisionTree.run(nodeSpan, _.runPattern(nodeSpan, left))
      left.runPattern(nodeSpan).orElse(right.runPattern(nodeSpan))
    end runPattern
  end alt

  private[forja] final class map[T, U](val pattern: Pattern[T], val fn: T => U)
      extends Pattern[U]:
    def runPattern(nodeSpan: NodeSpan): Option[(U, NodeSpan)] =
      pattern
        .runPattern(nodeSpan)
        .map: (value, nodeSpan) =>
          (fn(value), nodeSpan)
    end runPattern
    // FIXME: delete
    override def toString(): String = pattern.toString()
  end map

  private[forja] final class tokenExact[T](
      val token: Token,
      val pattern: Pattern[T],
  ) extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      val sizeToLeft = nodeSpan.size
      nodeSpan.expandRightOption(1) match
        case Some(nodeSpan) if nodeSpan.last.tokenOption.contains(token) =>
          pattern
            .runPattern(nodeSpan.last.children.asEmptyNodeSpan)
            .flatMap: (value, nodeSpan) =>
              nodeSpan.parentOption
                .map(_.singletonNodeSpanHere)
                .flatMap(_.expandLeftOption(sizeToLeft))
                .map((value, _))
        case None | Some(_) => None
    end runPattern
  end tokenExact

  private[forja] final class tokenAny[T](val pattern: Pattern[T])
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      val sizeToLeft = nodeSpan.size
      nodeSpan.expandRightOption(1) match
        case None           => None
        case Some(nodeSpan) =>
          pattern
            .runPattern(nodeSpan.last.children.asEmptyNodeSpan)
            .flatMap: (value, nodeSpan) =>
              nodeSpan.parentOption
                .map(_.singletonNodeSpanHere)
                .flatMap(_.expandLeftOption(sizeToLeft))
                .map((value, _))
    end runPattern
  end tokenAny

  private[forja] final class not[T](val pattern: Pattern[T])
      extends Pattern[Unit]:
    def runPattern(nodeSpan: NodeSpan): Option[(Unit, NodeSpan)] =
      pattern.runPattern(nodeSpan) match
        case None    => Some(((), nodeSpan))
        case Some(_) => None
    end runPattern
  end not

  private[forja] final class rewriteMap[T, U](
      val pattern: Pattern[T],
      val fn: T => (U, Node | Iterable[Node] | syntax.unchanged.type),
  ) extends Pattern[U]:
    def runPattern(nodeSpan: NodeSpan): Option[(U, NodeSpan)] =
      val elementsToTheLeft = nodeSpan.size
      pattern
        .runPattern(nodeSpan.drop(elementsToTheLeft))
        .map: (value, nodeSpan) =>
          val result = fn(value) match
            case (u, node: Node) =>
              (u, nodeSpan.replaceThis(List(node)))
            case (u, nodes: Iterable[Node]) =>
              (u, nodeSpan.replaceThis(nodes))
            case (u, syntax.unchanged) =>
              (u, nodeSpan)

          (result._1, result._2.expandLeftOption(elementsToTheLeft).get)
    end runPattern
  end rewriteMap

  private[forja] final class rep[T](val elem: Pattern[T])
      extends Pattern[List[T]]:
    def runPattern(nodeSpan: NodeSpan): Option[(List[T], NodeSpan)] =
      var shouldContinue = true
      var nodeSpanAcc = nodeSpan
      val resultAcc = mutable.ListBuffer[T]()

      while shouldContinue
      do
        elem.runPattern(nodeSpanAcc) match
          case None =>
            shouldContinue = false
          case Some((elem, nodeSpanAcc2)) =>
            resultAcc += elem
            nodeSpanAcc = nodeSpanAcc2
      end while

      Some((resultAcc.result(), nodeSpanAcc))
    end runPattern
  end rep

  private[forja] final class embed[T: Node.Embed] extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      nodeSpan
        .expandRightOption(1)
        .flatMap: nodeSpan =>
          nodeSpan.last.valueOption[T].map((_, nodeSpan))
    end runPattern
  end embed

  private[forja] final class EmbedLiteralPattern[T: Node.Embed](value: T)
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      nodeSpan
        .expandRightOption(1)
        .flatMap: nodeSpan =>
          nodeSpan.last.valueOption[T] match
            case Some(`value`) =>
              Some((value, nodeSpan))
            case None | Some(_) => None
    end runPattern
  end EmbedLiteralPattern

  private[forja] final class here[T](pattern: Pattern[Node], query: Query[T])
      extends Pattern[T]:
    def runPattern(nodeSpan: NodeSpan): Option[(T, NodeSpan)] =
      pattern
        .runPattern(nodeSpan)
        .flatMap: (node, nodeSpan) =>
          query.runQuery(node).value.map((_, nodeSpan))
    end runPattern
  end here
end Pattern
