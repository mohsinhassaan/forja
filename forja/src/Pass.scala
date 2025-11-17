package forja

import forja.util.ReflectiveEnumeration

import scala.annotation.tailrec

import Pass.*

trait Pass extends ReflectiveEnumeration.Enumerable:
  final def apply(root: Node): Node =
    if root.containsError
    then root
    else applyImpl(root)
  end apply

  protected def applyImpl(root: Node): Node
end Pass

object Pass:
  trait RewritePass extends Pass, ReflectiveEnumeration:
    private lazy val rewritesAgg: Pattern[Unit] =
      valuesByType[Query.rewrite[?]].view
        .map(_.pattern)
        .reduce(_ | _)
    end rewritesAgg

    final protected def applyImpl(node: Node): Node =
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
    end applyImpl
  end RewritePass

  trait MultiPass extends Pass, ReflectiveEnumeration:
    private lazy val passes = valuesByType[Pass]
    final protected def applyImpl(root: Node): Node =
      var node = root
      passes.foreach: pass =>
        if !node.containsError
        then node = pass(node)
      node
    end applyImpl
  end MultiPass
end Pass
