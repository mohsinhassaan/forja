package forja

import scala.compiletime.summonFrom

object syntax:
  export nodeSyntax.*
  export wfSyntax.*
  export patternSyntax.*
  export querySyntax.*

  object skipRewrite

  transparent inline def lit[T <: Matchable: Node.Embed](
      value: T,
  ): Node | Pattern[T] =
    summonFrom:
      case given PatternContext =>
        new Pattern.EmbedLiteralPattern(value)
      case _ =>
        Node.embed(value)
  end lit

  transparent inline def embed[T <: Matchable: Node.Embed]
      : Wf.EmbedWf[T] | Pattern[T] =
    summonFrom:
      case given PatternContext =>
        new Pattern.embed[T]
      case _ =>
        ??? // new Wf.EmbedWf[T]
  end embed

  object nodeSyntax:

  end nodeSyntax

  object wfSyntax:
    export Wf.TokenWf

    def rep(shapes: Wf.Shapes*): Wf.RepeatedShapeSeq =
      Wf.RepeatedShapeSeq(shapes*)
    end rep
  end wfSyntax

  object patternSyntax:
    def rep[T](elem: Pattern[T]): Pattern[List[T]] =
      Pattern.rep(elem)
    end rep

    def rep1[T](elem: Pattern[T])(using PatternContext): Pattern[List[T]] =
      NodeSpan(+elem, +rep(elem)).map(_ :: _)
    end rep1

    def not[T](elem: Pattern[T])(using PatternContext): Pattern[Unit] =
      Pattern.not(elem)
    end not
  end patternSyntax

  object querySyntax:
    export Query.on
  end querySyntax
end syntax
