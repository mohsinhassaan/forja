package forja.langs.calc

import forja.Node.NodeSpan
import forja.syntax.*
import forja.{Node, Pass, Token}

object CalcParser extends Pass.MultiPass:
  import CalcReader.Tokenized
  lazy val ParseHead = Token()
  lazy val ParseLimit = Token()
  lazy val ParseOngoing = Token()
  def reader = CalcReader

  object addParseHead extends Pass.RewritePass:
    def addParseHead = on(
      Tokenized.Root(
        NodeSpan(cc.not(ParseHead(`...`))).rewrite: _ =>
          NodeSpan(ParseHead(0)),
        `...`,
      ),
    ).rewriteInPattern
  end addParseHead

  object parseAST extends Pass.RewritePass:
    def parseNumber = on(
      !ParseHead(cc.embed[Int]),
      +Tokenized.Number(+cc.embed[Int]),
    ).rewrite: (hd, num) =>
      NodeSpan(
        CalcAST.Expression(CalcAST.Number(num)),
        hd,
      )

    def parseRootLimit = on(
      Tokenized.Root(
        `...`,
        ParseHead(cc.embed[Int]).rewrite: _ =>
          ParseLimit(),
      ),
    ).rewriteInPattern

    def parseGroupIn = on(
      +ParseHead(+cc.embed[Int]),
      !Tokenized.Group(
        NodeSpan().rewrite: _ =>
          ParseHead(0),
        `...`,
      ),
    ).rewrite: (prec, grp) =>
      NodeSpan(
        ParseOngoing(prec),
        grp,
      )

    def parseGroupLimit = on(
      ParseOngoing(`...`),
      Tokenized.Group(
        `...`,
        ParseHead(cc.embed[Int]).rewrite: _ =>
          ParseLimit(),
      ),
    ).rewriteInPattern

    def parseGroupOut = on(
      +ParseOngoing(+cc.embed[Int]),
      +Tokenized.Group(
        !CalcAST.Expression(`...`),
        ParseLimit(),
      ),
    ).rewrite: (prec, expr) =>
      NodeSpan(expr, ParseHead(prec))

    def parseAddSubIn = on(
      !CalcAST.Expression(`...`),
      +ParseHead(+cc.embed[Int].filter(_ <= 1)),
      !(Tokenized.Add() | Tokenized.Sub()),
    ).rewrite: (lhs, prec, op) =>
      NodeSpan(
        lhs,
        ParseOngoing(prec),
        op,
        ParseHead(1),
      )

    def parseAddSubOut = on(
      !CalcAST.Expression(`...`),
      +ParseOngoing(+cc.embed[Int]),
      !(Tokenized.Add() | Tokenized.Sub()),
      !CalcAST.Expression(`...`),
      ParseLimit(),
    ).rewrite: (lhs, prec, op, rhs) =>
      NodeSpan(
        CalcAST.Expression(
          op.tokenOption.get(
            lhs,
            rhs,
          ),
        ),
        ParseHead(prec),
      )

    def parseMulDivIn = on(
      !CalcAST.Expression(`...`),
      +ParseHead(+cc.embed[Int]),
      !(Tokenized.Mul() | Tokenized.Div()),
    ).rewrite: (lhs, prec, op) =>
      NodeSpan(
        lhs,
        ParseOngoing(prec),
        op,
        ParseHead(2),
      )

    def parseMulDivOut = on(
      !CalcAST.Expression(`...`),
      +ParseOngoing(+cc.embed[Int]),
      !(Tokenized.Mul() | Tokenized.Div()),
      !CalcAST.Expression(`...`),
      ParseLimit(),
    ).rewrite: (lhs, prec, op, rhs) =>
      NodeSpan(
        CalcAST.Expression(
          op.tokenOption.get(
            lhs,
            rhs,
          ),
        ),
        ParseHead(prec),
      )

    def parseMulDivLimit = on(
      +ParseHead(+cc.embed[Int].filter(_ >= 2)),
      !(Tokenized.Add() | Tokenized.Sub()),
    ).rewrite: (prec, op) =>
      NodeSpan(
        ParseLimit(),
        op,
      )
  end parseAST

  object stripMeta extends Pass.RewritePass:
    def successCondition = on(
      +Tokenized.Root(
        !CalcAST.Expression(`...`),
        ParseLimit(),
      ),
    ).rewrite: expr =>
      // TODO: why does removing NodeSpan cause infinite loop (???)
      NodeSpan(expr)

    def unexpectedEmptyInput = on(
      !Tokenized.Root(
        ParseLimit(),
      ),
    ).rewrite: node =>
      Node.error(s"input is empty")(node)

    def unexpectedOperator = on(
      !ParseHead(cc.embed[Int]),
      !(Tokenized.Add() | Tokenized.Sub() | Tokenized.Mul() | Tokenized.Div()),
    ).rewrite: (hd, op) =>
      Node.error(s"unexpected operator")(hd, op)

    def missingRhs = on(
      !(Tokenized.Add() | Tokenized.Sub() | Tokenized.Mul() | Tokenized.Div()),
      !ParseLimit(),
    ).rewrite: (op, lm) =>
      Node.error(s"missing rhs")(op, lm)

    def unexpectedEmptyGroup = on(
      !Tokenized.Group(
        CalcParser.ParseLimit(),
      ),
    ).rewrite: grp =>
      Node.error(s"empty group")(grp)

    def tooManyExpressions = on(
      !CalcAST.Expression(`...`),
      +cc.rep1(NodeSpan(!CalcAST.Expression(`...`))),
      !ParseLimit(),
    ).rewrite: (expr, exprs, lm) =>
      Node.error(s"too many expressions")(((expr +: exprs) :+ lm)*)
  end stripMeta

  def validateAST = CalcAST.Expression.validate
end CalcParser
