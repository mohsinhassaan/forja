package forja

import scala.collection.concurrent
import scala.util.NotGiven

final class Token private (val fullName: String):
  transparent inline def apply(inline args: Node.NodeApplyArg*)(using
      NotGiven[PatternContext],
  ): Node =
    Node.apply(this, args*)

  transparent inline def apply(inline args: Node.PatternApplyArg*)(using
      inline ctx: PatternContext,
  ): Pattern[Tuple] =
    Node.apply(this, args*)
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
end Token
