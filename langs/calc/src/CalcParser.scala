package forja.langs.calc

import forja.Node.NodeSpan
import forja.syntax.*
import forja.{Node, Pass, Token}

object CalcParser extends Pass.MultiPass:
  import CalcReader.Tokenized
  lazy val ParseHead = Token()
  lazy val ParseLimit = Token()
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

    def parseGroupIn = on(
      +ParseHead(+cc.embed[Int]),
      !Tokenized.Group(
        NodeSpan().rewrite: _ =>
          ParseHead(0),
        `...`,
      ),
    ).rewrite: (prec, grp) =>
      NodeSpan(
        ParseHead(
          prec,
          grp,
        ),
      )

    def parseGroupLimit = on(
      ParseHead(
        cc.embed[Int],
        Tokenized.Group(
          `...`,
          ParseHead(cc.embed[Int]).rewrite: _ =>
            ParseLimit(),
        ),
      ),
    ).rewriteInPattern

    def parseGroupOut = on(
      +ParseHead(
        +cc.embed[Int],
        +Tokenized.Group(
          !CalcAST.Expression(`...`),
          ParseLimit(),
        ),
      ),
    ).rewrite: (prec, expr) =>
      NodeSpan(expr, ParseHead(prec))

    def parseAddSubIn = on(
      !CalcAST.Expression(`...`),
      +ParseHead(+cc.embed[Int].filter(_ <= 1)),
      !(Tokenized.Add() | Tokenized.Sub()),
    ).rewrite: (lhs, prec, op) =>
      NodeSpan(
        ParseHead(prec, lhs, op),
        ParseHead(1),
      )

    // def parseAddSubConsume = on(
    //   !CalcAST.Expression(`...`),
    //   +ParseExpect(+cc.embed[Int]),
    //   !(Tokenized.Add() | Tokenized.Sub()),
    //   !CalcAST.Expression(`...`),
    //   !ParseHead(cc.lit(1)),
    // ).rewrite: (lhs, prec, op, rhs) =>
    //   NodeSpan(
    //     CalcAST.Expression(
    //       Node(
    //         if op.tokenOption == Some(Tokenized.Add.token)
    //         then CalcAST.Add.token
    //         else CalcAST.Sub.token,
    //         lhs,
    //         rhs,
    //       ),
    //     ),
    //     hd,
    //   )

    // def parseMulDivReach = on(
    //   !CalcAST.Expression(`...`),
    //   !ParseHead(cc.embed[Int].filter(_ <= 2)),
    //   !(Tokenized.Mul() | Tokenized.Div()),
    //   cc.not(CalcAST.Expression(`...`) | ParseHead(`...`)),
    // ).rewrite: (lhs, hd, op) =>
    //   NodeSpan(
    //     lhs,
    //     hd,
    //     op,
    //     ParseHead(2),
    //   )
  end parseAST

  object stripMeta extends Pass.RewritePass:
    def headAtEndOfRootIsOk = on(
      +Tokenized.Root(
        !CalcAST.Expression(`...`),
        ParseHead(cc.lit(0)),
      ),
    ).rewrite: expr =>
      // TODO: why does removing NodeSpan cause infinite loop (???)
      NodeSpan(expr)
  end stripMeta

  def validateAST = CalcAST.Expression.validate
end CalcParser
