package forja.langs.calc

import forja.Wf.TokenWf
import forja.syntax.*
import forja.{Node, Pass, SourceRange, Token, Wf}

object CalcReader extends Pass.MultiPass:
  trait Input extends Wf:
    lazy val Root = Token(
      ParseHead,
      embed[SourceRange],
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

    def popByte = on(
      !Input.ParseHead(),
      +embed[SourceRange].filter(_.nonEmpty),
    ).rewrite: (hd, rng) =>
      List(hd, lit(rng.head), lit(rng.tail))
    end popByte

    def skipWhitespace = on(
      !Input.ParseHead(),
      lit(' '.toByte) | lit('\n'.toByte),
    ).rewrite: hd =>
      hd
    end skipWhitespace

    def parseToken = on(
      !Input.ParseHead(),
      +(tokenBytes.map((b, tok) => lit(b).map((_, tok))).reduce(_ | _)),
    ).rewrite: (hd, p) =>
      List(p._2(), hd)
    end parseToken

    def doneReading = on(
      Input.ParseHead(),
      embed[SourceRange].filter(_.isEmpty),
    ).rewrite: _ =>
      Nil
    end doneReading
  end readTokens

  trait Tokenized extends Input, CalcAST:
    override lazy val Root = Input.Root.replace(
      rep(
        Number
          | Add
          | Sub
          | Mul
          | Div,
      ),
    )
    override lazy val Add = CalcAST.Add.replace()
    override lazy val Sub = CalcAST.Sub.replace()
    override lazy val Mul = CalcAST.Mul.replace()
    override lazy val Div = CalcAST.Div.replace()
  end Tokenized

  object Tokenized extends Tokenized
  def validateTokenizedRoot = Tokenized.Root.validate
end CalcReader
