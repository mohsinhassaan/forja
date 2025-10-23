package forja

import scala.annotation.publicInBinary
import scala.collection.concurrent
import scala.quoted.{Expr, Quotes, Varargs}

final class Token private (val fullName: String):
  transparent inline def apply(inline args: Any*): Node | Pattern[Any] =
    ${ Token.tokenApplyImpl('this, 'args) }
  end apply

  override def toString(): String = fullName
end Token

object Token:
  private val byNameStore = concurrent.TrieMap[String, Token]()
  def byName(fullName: String): Token =
    byNameStore.getOrElseUpdate(fullName, new Token(fullName))
  end byName

  def apply(shapes: => Wf.Shapes*)(using
      fullName: sourcecode.FullName,
  ): Wf.TokenWf =
    Wf.TokenWf(byName(fullName.value), Wf.ShapeSeq(shapes*))
  end apply

  @publicInBinary
  private[forja] def tokenApplyImpl(
      tokenExpr: Expr[Token],
      argsExpr: Expr[Seq[Any]],
  )(using Quotes): Expr[Node | Pattern[Any]] =
    argsExpr match
      case Varargs(argExprs) =>
        Node.applyImpl(Varargs(tokenExpr +: argExprs))
  end tokenApplyImpl
end Token
