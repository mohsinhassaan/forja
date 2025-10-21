package forja

import Wf.*

trait Wf:

end Wf

object Wf:
  sealed trait Shape:
    def |(other: TokenWf): Shape
  end Shape

  final class TokenWf private[forja] (val token: Token, shapeSeq: => ShapeSeq)
      extends Shape,
        Pass:
    def |(other: TokenWf): Shape = Choice(this, other)

    private lazy val stableShapeSeq = shapeSeq

    protected def applyImpl(root: Node): Node = ???
  end TokenWf

  private final class Choice(tokens: TokenWf*) extends Shape:
    def |(other: TokenWf): Shape = Choice((tokens :+ other)*)
  end Choice

  type Shapes = Shape | (Token, Shape) | RepeatedShapeSeq

  private[forja] final class ShapeSeq(val shapes: Shapes*)

  private[forja] final class RepeatedShapeSeq private[forja] (
      val shapes: Shapes*,
  )

  def rep(shapes: Shapes*): RepeatedShapeSeq =
    RepeatedShapeSeq(shapes*)
  end rep

  given Conversion[TokenWf, Token] = _.token
end Wf
