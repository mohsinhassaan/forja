package forja

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes

sealed abstract class Context

object Context:
  case object PatternContext extends Context
  type PatternContext = PatternContext.type
  case object ValueContext extends Context
  type ValueContext = ValueContext.type

  given default: ValueContext = ValueContext

  private[forja] transparent inline def dispatch[T, U](inline patFn: PatternContext ?=> T, vFn: ValueContext ?=> U): T | U =
    ${ dispatchImpl('patFn, 'vFn) }
  end dispatch
  
  private[forja] def dispatchImpl[T : Type, U : Type](patFn: =>Expr[PatternContext ?=> T], vFn: =>Expr[ValueContext ?=> U])(using Quotes): Expr[T | U] =
    Expr.summon[Context] match
      case Some(expr) if expr.isExprOf[PatternContext] =>
        Expr.betaReduce('{ $patFn(using PatternContext) })
      case Some(expr) if expr.isExprOf[ValueContext] =>
        Expr.betaReduce('{ $vFn(using ValueContext) })
      case Some(expr) =>
        quotes.reflect.report.warning(s"${expr.show} is not a specific context. Defaulting to value")
        Expr.betaReduce('{ $vFn(using ValueContext) })
      case None =>
        quotes.reflect.report.errorAndAbort("no Context found! This should never happen because there is a default one")
  end dispatchImpl
end Context
