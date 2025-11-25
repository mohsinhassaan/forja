package forja.langs.calc

import forja.Wf.{Shape, TokenWf}
import forja.syntax.*
import forja.{Node, NodeSpan, Pass, SourceRange, Token, Wf}

object CalcReader extends Pass.MultiPass:
  trait Input extends Wf:
    lazy val Root = Token(
      ParseHead,
      cc.embed[SourceRange],
    )
    lazy val ParseHead = Token()
  end Input

  object Input extends Input
  def validateInputRoot = Input.Root.validate

  object readTokens extends Pass.RewritePass:
    private val tokenBytes = List[(Char, TokenWf)](
      '+' -> Tokenized.Add,
      '-' -> Tokenized.Sub,
      '*' -> Tokenized.Mul,
      '/' -> Tokenized.Div,
    ).map((b, wf) => b.toByte -> wf.token)

    private val numberBytes = ('0' to '9').map(_.toByte).toSet

    def popByte = on(
      Input.ParseHead(),
      cc.embed[SourceRange]
        .filter(_.nonEmpty)
        .rewrite: rng =>
          if numberBytes(rng.head)
          then
            NodeSpan(
              rng.head,
              rng.tail.iterator.takeWhile(numberBytes).map(cc.lit),
              rng.tail.dropWhile(numberBytes),
            )
          else NodeSpan(rng.head, rng.tail),
    ).rewriteInPattern
    end popByte

    def skipWhitespace = on(
      !Input.ParseHead(),
      cc.lit(' '.toByte, '\n'.toByte, '\t'.toByte, '\r'.toByte),
    ).rewrite: hd =>
      hd
    end skipWhitespace

    def openGroup = on(
      !Input.ParseHead(),
      cc.lit('('.toByte),
      +cc.embed[SourceRange],
    ).rewrite: (hd, rng) =>
      Tokenized.Group(
        hd,
        rng,
      )
    end openGroup

    def closeGroup = on(
      !Tokenized.Group(
        `...`,
        +NodeSpan(
          !Input.ParseHead(),
          cc.lit(')'.toByte),
          +cc.embed[SourceRange],
        ).rewriteMap: p =>
          (p, NodeSpan()),
      ),
    ).rewrite: (g, hd, rng) =>
      NodeSpan(g, hd, rng)
    end closeGroup

    def parseToken = on(
      !Input.ParseHead(),
      +(tokenBytes.map((b, tok) => cc.lit(b).map((_, tok))).reduce(_ | _)),
    ).rewrite: (hd, _, tok) =>
      NodeSpan(tok(), hd)
    end parseToken

    def doneReading = on(
      Input.ParseHead(),
      cc.embed[SourceRange].filter(_.isEmpty),
    ).rewrite: _ =>
      NodeSpan()
    end doneReading

    def readNumber = on(
      !Input.ParseHead(),
      +cc.rep1(cc.embed[Byte].filter(numberBytes)),
      +cc.embed[SourceRange],
    ).rewrite: (hd, bytes, last) =>
      val num: Int = bytes.iterator.map(_.toChar).mkString.toInt
      NodeSpan(Tokenized.Number(num), hd, last)
    end readNumber
  end readTokens

  object errorCases extends Pass.RewritePass:
    def unmatchedClosingBrace = on(
      !Input.ParseHead(),
      +cc.lit(')'.toByte),
    ).rewrite: (hd, close) =>
      Node.error(s"unmatched closing brace")(hd, Node.embed(close))

    def invalidChar = on(
      !Input.ParseHead(),
      +cc.embed[Byte].filter(_ != ')'),
    ).rewrite: (hd, ch) =>
      Node.error(s"invalid byte")(hd, Node.embed(ch))
  end errorCases

  trait Tokenized extends Input, CalcAST:
    def anyTok: Shape =
      Number
        | Add
        | Sub
        | Mul
        | Div
        | Group
    end anyTok
    override lazy val Root = Input.Root.replace(
      cc.rep(anyTok),
    )
    override lazy val Add = CalcAST.Add.replace()
    override lazy val Sub = CalcAST.Sub.replace()
    override lazy val Mul = CalcAST.Mul.replace()
    override lazy val Div = CalcAST.Div.replace()
    lazy val Group = Token(
      cc.rep(anyTok),
    )
  end Tokenized

  object Tokenized extends Tokenized
  def validateTokenizedRoot = Tokenized.Root.validate
end CalcReader
