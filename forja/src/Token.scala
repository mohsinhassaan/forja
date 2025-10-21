package forja

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.collection.concurrent

final class Token private (val fullName: String):
  transparent inline def apply(inline args: Node.NodeApplyArg*): Node =
    ${ Token.applyImpl('this, 'args) }
end Token

object Token:
  private val byNameStore = concurrent.TrieMap[String, Token]()
  def byName(fullName: String): Token =
    byNameStore.getOrElseUpdate(fullName, new Token(fullName))
  end byName

  // def apply()

  def applyImpl(tokenExpr: Expr[Token], args: Expr[Seq[Node.NodeApplyArg]])(using Quotes): Expr[Node] =
    '{ Node.apply($tokenExpr, $args*) }
  end applyImpl
end Token
