package forja

import scala.compiletime.summonFrom
import scala.util.NotGiven

object syntax:
  export nodeSyntax.*
  export wfSyntax.*
  export patternSyntax.*
  export querySyntax.*

  transparent inline def lit[T <: Matchable: Node.Embed](
      value: T,
  ): Node | Pattern[T] =
    summonFrom:
      case given PatternContext =>
        new Pattern.EmbedLiteralPattern(value)
      case _ =>
        Node.embed(value)
  end lit

  object nodeSyntax:

  end nodeSyntax

  object wfSyntax:
    export Wf.TokenWf

    def rep(shapes: Wf.Shapes*): Wf.RepeatedShapeSeq =
      Wf.RepeatedShapeSeq(shapes*)
    end rep

    def embed[T: Node.Embed](using NotGiven[PatternContext]): Wf.EmbedWf =
      Wf.EmbedWf(summon[Node.Embed[T]])
    end embed
  end wfSyntax

  object patternSyntax:
    def embed[T <: Matchable: Node.Embed](using PatternContext): Pattern[T] =
      new Pattern.embed[T]

    def rep[T](elem: Pattern[T]): Pattern[List[T]] =
      Pattern.rep(elem)
    end rep

    def rep1[T](elem: Pattern[T])(using PatternContext): Pattern[List[T]] =
      NodeSpan(+elem, +rep(elem)).map(_ :: _)
    end rep1
  end patternSyntax

  object querySyntax:
    export Query.on
  end querySyntax
end syntax
