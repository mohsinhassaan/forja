package forja

import scala.annotation.publicInBinary

import Wf.*

trait Wf:
  protected given localContext: syntax.WfContext.type = syntax.WfContext
end Wf

object Wf:
  sealed trait Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape
  end Shape

  final class EmbedWf[T <: Matchable] @publicInBinary private[forja] (using
      val embed: Node.Embed[T],
  ) extends Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape =
      Choice(this, other)
    end |
  end EmbedWf

  final class TokenWf private[forja] (val token: Token, shapeSeq: => ShapeSeq)
      extends Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape = Choice(this, other)

    export token.apply

    def replace(shapes: => Shapes*): TokenWf =
      new TokenWf(token, ShapeSeq(shapes*))
    end replace

    private lazy val stableShapeSeq = shapeSeq

    object validate extends Pass:
      protected def applyImpl(root: Node): Node =
        // TODO: validate
        root
    end validate
  end TokenWf

  private final class Choice(tokens: TokenWf | EmbedWf[?]*) extends Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape = Choice((tokens :+ other)*)
  end Choice

  type Shapes = Shape | (Token, Shape) | RepeatedShapeSeq

  private[forja] final class ShapeSeq(val shapes: Shapes*)

  private[forja] final class RepeatedShapeSeq @publicInBinary private[forja] (
      val shapes: Shapes*,
  )
end Wf
