package forja

object syntax:
  export nodeSyntax.*
  export wfSyntax.*
  export patternSyntax.*
  export querySyntax.*

  object skipRewrite

  def ctx[C <: Context](c: C): c.type = c
  

  transparent inline def lit[T <: Matchable: Node.Embed](
      value: T,
  ): Node | Pattern[T] =
    Context.dispatch(
      patFn = new Pattern.EmbedLiteralPattern(value),
      vFn = Node.embed(value),
    )
  end lit

  transparent inline def embed[T <: Matchable: Node.Embed]
      : Wf.EmbedWf[T] | Pattern[T] =
    Context.dispatch(
      patFn = new Pattern.embed[T],
      vFn = ???,
    )
  end embed

  transparent inline def rep[T](
      elem: Pattern[T] | Wf.Shapes,
  ): Pattern[List[T]] | Wf.RepeatedShapeSeq =
    inline elem match
      case elem: Pattern[T] =>
        Pattern.rep(elem)
      case elem: Wf.Shapes =>
        ??? // new Wf.RepeatedShapeSeq(Seq(elem))
  end rep

  // The glob pattern, match any number of nodes --> NodeSpan
  // extension? Can put a condition on which nodes matched 
  // Tricky semantics:
  // - if there is a pattern after, must expand until pattern does not match
  // - if no pattern after, match everything
  // - same with unguarded prefix, but match from RHS of node span (reversed match order!)

  case object `...`
  type `...` = `...`.type
  
  object nodeSyntax:

  end nodeSyntax

  object wfSyntax:
    export Wf.TokenWf
  end wfSyntax

  object patternSyntax:
    def rep1[T](elem: Pattern[T])(using Context.PatternContext): Pattern[List[T]] =
      NodeSpan(+elem, +rep(elem)).map(_ :: _)
    end rep1

    def not[T](elem: Pattern[T])(using Context.PatternContext): Pattern[Unit] =
      Pattern.not(elem)
    end not
  end patternSyntax

  object querySyntax:
    export Query.on
  end querySyntax
end syntax
