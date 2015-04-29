package calculator

import  scala.util.parsing.combinator._
object MathParser extends JavaTokenParsers {

  def expr: Parser[Expr] =
    (term ~ "+" ~ term) ^^ { case lhs ~ plus ~ rhs => BinaryOp("+", lhs, rhs) } |
      (term ~ "-" ~ term) ^^ { case lhs ~ minus ~ rhs => BinaryOp("-", lhs, rhs) } |
      (term ~ "!" ~ "[+|-]".r ~ term) ^^ { case lhs ~ factorial ~ sign ~ rhs => BinaryOp(sign, UnaryOp("!",lhs), rhs) } |
      (term ~ "[+|-]".r ~ term ~ "!") ^^ { case lhs ~ sign ~ rhs ~ factor => BinaryOp(sign, lhs, UnaryOp("!", rhs)) } |
      ("sin(" ~>term <~ ")") ^^ {  case x => UnaryOp("sin", x) } |
      ("cos(" ~>term  <~ ")") ^^ {  case x => UnaryOp("cos", x) } |
      (term <~ "!") ^^ { case x => UnaryOp("!", x) } |
      ("-" ~> term) ^^ { case x => UnaryOp("-", x) } |
      term

  def term: Parser[Expr] =
    (factor ~ "!" ~ "[*|/|^]".r ~ factor) ^^ { case lhs ~ factorial ~ sign ~ rhs => BinaryOp(sign, UnaryOp("!",lhs), rhs) } |
      (factor ~ "[*|/|^]".r ~ factor ~ "!") ^^ { case lhs ~ sign ~ rhs ~ factorial => BinaryOp(sign, lhs, UnaryOp("!", rhs))} |
      (factor ~ "*" ~ factor) ^^ { case lhs ~ times ~ rhs => BinaryOp("*", lhs, rhs) } |
      (factor ~ "^" ~ factor) ^^ { case lhs ~ pow ~ rhs => BinaryOp("^", lhs, rhs) } |
      (factor ~ "/" ~ factor) ^^ { case lhs ~ div ~ rhs => BinaryOp("/", lhs, rhs) } |
      ("-" ~> factor) ^^ { case x => UnaryOp("-", x) } |
      ("-(" ~ factor ~ "[*|/|^]".r ~ factor ~ ")") ^^ { case _ ~ lh ~ sign ~ rh ~ _ => UnaryOp("-", BinaryOp(sign, lh, rh)) } |
      factor

  def factor: Parser[Expr] =
    ("sin(" ~>expr <~ ")") ^^ {  case x => UnaryOp("sin", x) } |
      ("cos(" ~>expr  <~ ")") ^^ {  case x => UnaryOp("cos", x) } |
      ("-(" ~> expr <~ ")") ^^ { case x => UnaryOp("-", x) } |
      "(" ~> expr <~ ")" |
      "-" ~> expr |
      floatingPointNumber ^^ { x => Number(x.toFloat) }

  def parse(text: String) = {
    parseAll(expr, text) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

