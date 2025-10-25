package forja.langs.calc

import forja.Wf.{Shape, TokenWf}
import forja.syntax.*
import forja.{Node, NodeSpan, Pass, SourceRange, Token, Wf}

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

    private val numberBytes = ('0' to '9').map(_.toByte).toSet

    def popByte = on(
      !Input.ParseHead(),
      +rep(embed[Byte].filter(numberBytes)),
      +embed[SourceRange].filter(_.nonEmpty),
    ).rewrite: (hd, bytes, rng) =>
      NodeSpan(hd, bytes.map(lit), rng.head, rng.tail)
    end popByte

    def skipWhitespace = on(
      !Input.ParseHead(),
      lit(' '.toByte) | lit('\n'.toByte) | lit('\t'.toByte),
    ).rewrite: hd =>
      hd
    end skipWhitespace

    def openGroup = on(
      !Input.ParseHead(),
      lit('('.toByte),
      +embed[SourceRange],
    ).rewrite: (hd, rng) =>
      Tokenized.Group(
        hd,
        rng,
      )
    end openGroup

    def closeGroup = on(
      !Tokenized.Group(
        rep(NodeSpan(not(Input.ParseHead()), Node())),
        !Input.ParseHead(),
        lit(')'.toByte),
        +embed[SourceRange],
      ),
    ).rewrite: (g, hd, rng) =>
      NodeSpan(
        Tokenized.Group(g.children.view.dropRight(3)),
        hd,
        rng,
      )
    end closeGroup

    def parseToken = on(
      !Input.ParseHead(),
      +(tokenBytes.map((b, tok) => lit(b).map((_, tok))).reduce(_ | _)),
    ).rewrite: (hd, p) =>
      NodeSpan(p._2(), hd)
    end parseToken

    def doneReading = on(
      Input.ParseHead(),
      embed[SourceRange].filter(_.isEmpty),
    ).rewrite: _ =>
      NodeSpan()
    end doneReading

    def readNumber = on(
      !Input.ParseHead(),
      +rep1(embed[Byte].filter(numberBytes)),
      +(embed[Byte].filter(b => !numberBytes(b)).map(Node.embed)
        | embed[SourceRange].filter(_.isEmpty).map(Node.embed)),
    ).rewrite: (hd, bytes, last) =>
      val num: Int = bytes.iterator.map(_.toChar).mkString.toInt
      NodeSpan(Tokenized.Number(num), hd, last)
    end readNumber
  end readTokens

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
      rep(anyTok),
    )
    override lazy val Add = CalcAST.Add.replace()
    override lazy val Sub = CalcAST.Sub.replace()
    override lazy val Mul = CalcAST.Mul.replace()
    override lazy val Div = CalcAST.Div.replace()
    lazy val Group = Token(
      rep(anyTok),
    )
  end Tokenized

  object Tokenized extends Tokenized
  def validateTokenizedRoot = Tokenized.Root.validate
end CalcReader
