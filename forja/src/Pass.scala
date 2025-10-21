package forja

import Pass.*

// Rewrite pass
// Validate pass
// Composite pass (?)

// pass is from Node => Either[Node, Node], stop if empty

trait Pass:
  def rewrites: List[Rewrite]

  private lazy val combinedRewrites =
    combineRewrites(rewrites)
  final def apply(root: Node): Either[Node, Node] =
    ???
  end apply
end Pass

object Pass:
  opaque type Rewrite = Pattern[(Any, (NodeSpan, Any) => NodeSpan)]

  private[Pass] def combineRewrites(rewrites: List[Rewrite]): Rewrite =
    require(rewrites.nonEmpty)
    rewrites.reduce(_ | _)

  private def applyRewrite(rewrite: Rewrite, nodeSpan: NodeSpan): Option[NodeSpan] =
    ???
end Pass
