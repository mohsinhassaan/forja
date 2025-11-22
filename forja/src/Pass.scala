package forja

import forja.Wf.TokenWf
import forja.util.ReflectiveEnumeration

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

import Pass.*

trait Pass extends ReflectiveEnumeration.Enumerable:
  final def perform(root: Node): Node =
    if root.containsError
    then root
    else performImpl(root)
  end perform

  protected def performImpl(root: Node): Node
end Pass

object Pass:
  trait RewritePass extends Pass, ReflectiveEnumeration:
    private lazy val rewritesAgg: Pattern[Unit] =
      valuesByType[Query.rewrite[?]].view
        .map(_.pattern)
        .reduce(_ | _)
    end rewritesAgg

    final protected def performImpl(node: Node): Node =
      @tailrec
      def impl(
          nodeSpan: NodeSpan,
          madeChangesThisIteration: Boolean,
      ): NodeSpan =
        rewritesAgg.runPattern(nodeSpan) match
          case None =>
            assert(nodeSpan.isEmpty)
            def firstChild = nodeSpan
              .expandRightOption(1)
              .map(_.head.children.asEmptyNodeSpan)
            end firstChild

            def firstAvailableParentsSiblingOrRoot(
                nodeSpan: NodeSpan,
            ): NodeSpan =
              nodeSpan.parentOption match
                case None         => nodeSpan
                case Some(parent) =>
                  parent.rightSiblingOption match
                    case None =>
                      firstAvailableParentsSiblingOrRoot(
                        parent.emptyNodeSpanHere,
                      )
                    case Some(sibling) => sibling.emptyNodeSpanHere
            end firstAvailableParentsSiblingOrRoot

            val nextSpan =
              firstChild.getOrElse(firstAvailableParentsSiblingOrRoot(nodeSpan))
            if nextSpan.parentOption.isEmpty
            then
              if madeChangesThisIteration
              then impl(nextSpan, madeChangesThisIteration = false)
              else nextSpan
            else impl(nextSpan, madeChangesThisIteration)
          case Some(((), nodeSpan)) =>
            impl(nodeSpan.take(0), madeChangesThisIteration = true)
      end impl

      val nodeSpan =
        impl(node.asOrphan.emptyNodeSpanHere, madeChangesThisIteration = false)
      val replacementNode =
        nodeSpan.headOption
          .getOrElse(nodeSpan.expandRightOption(1).get.head)
      // Put our parents back
      node.replaceThis(replacementNode)
    end performImpl

    trait ModelChecker extends forja.ModelChecker:
      def inputWf: TokenWf
      def outputWf: TokenWf

      type State = Node

      extension (state: Node)
        final def isErrorState: Boolean =
          !state.containsError
            && outputWf.validate
              .perform(state)
              .containsError
        end isErrorState
      end extension

      def initStates(using ExecutionContext): Iterator[Node] =
        // map from token to combination set
        // --> rebuild map from nodes at all prev lvls
        // there isn't actually much recursion, just a data driven recurrence
        inputWf
        // ideas:
        // - limit width using custom rules, default behavior is to iterate "level"s
        // - limit number of init states using takeWhile type logic

        // custom rules could be found using ReflectiveEnumeration?
        ???
      end initStates

      def nextStates(state: Node)(using ExecutionContext): Iterator[Node] =
        // ideas:
        // - detect infinite loops (recurrent rule applications that return to the same state)
        // - rewrite rules applied in every position at once, find all interpretations
        ???
      end nextStates
    end ModelChecker
  end RewritePass

  trait MultiPass extends Pass, ReflectiveEnumeration:
    private lazy val passes = valuesByType[Pass]
    final protected def performImpl(root: Node): Node =
      var node = root
      passes.foreach: pass =>
        if !node.containsError
        then node = pass.perform(node)
      node
    end performImpl
  end MultiPass
end Pass
