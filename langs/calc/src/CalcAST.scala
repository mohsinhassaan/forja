package forja.langs.calc

import forja.syntax.*
import forja.{Token, Wf}

trait CalcAST extends Wf:
  lazy val Expression: TokenWf = Token(
    Number
      | Add
      | Sub
      | Mul
      | Div,
  )
  lazy val Number = Token(cc.embed[Int])
  lazy val Add = Token(
    Expression,
    Expression,
  )
  lazy val Sub = Token(
    Expression,
    Expression,
  )
  lazy val Mul = Token(
    Expression,
    Expression,
  )
  lazy val Div = Token(
    Expression,
    Expression,
  )
end CalcAST

object CalcAST extends CalcAST
