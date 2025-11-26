package forja.langs.calc

import forja.Node.NodeSpan
import forja.syntax.*
import forja.{Node, Pass}

object CalcEvaluator extends Pass.MultiPass:
  def validateInput = CalcAST.Expression.validate

  object evaluateExpr extends Pass.RewritePass:
    def evalAdd = on(
      CalcAST.Expression(
        CalcAST
          .Add(
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
          )
          .rewrite: (lhs, rhs) =>
            CalcAST.Number(lhs + rhs),
      ),
    ).rewriteInPattern

    def evalSub = on(
      CalcAST.Expression(
        CalcAST
          .Sub(
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
          )
          .rewrite: (lhs, rhs) =>
            CalcAST.Number(lhs - rhs),
      ),
    ).rewriteInPattern

    def evalMul = on(
      CalcAST.Expression(
        CalcAST
          .Mul(
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
          )
          .rewrite: (lhs, rhs) =>
            CalcAST.Number(lhs * rhs),
      ),
    ).rewriteInPattern

    def evalDivNonZero = on(
      CalcAST.Expression(
        CalcAST
          .Div(
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int])),
            +CalcAST.Expression(+CalcAST.Number(+cc.embed[Int].filter(_ != 0))),
          )
          .rewrite: (lhs, rhs) =>
            CalcAST.Number(lhs / rhs),
      ),
    ).rewriteInPattern

    def evalDivByZeroErr = on(
      CalcAST.Expression(
        CalcAST.Div(
          CalcAST.Expression(CalcAST.Number(cc.embed[Int])),
          NodeSpan(
            !CalcAST.Expression(CalcAST.Number(cc.lit(0))),
          ).rewrite: rhs =>
            Node.error("division by 0")(rhs),
        ),
      ),
    ).rewriteInPattern
  end evaluateExpr

  trait Evaluated extends CalcAST:
    override lazy val Expression: TokenWf =
      CalcAST.Expression.replace(CalcAST.Number)
  end Evaluated
  object Evaluated extends Evaluated

  def validateOutput = Evaluated.Expression.validate
end CalcEvaluator
