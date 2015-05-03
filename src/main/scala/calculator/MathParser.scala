package calculator

import  scala.util.parsing.combinator._
object MathParser extends JavaTokenParsers {

  def expr: Parser[Expr] =
    (term ~ "+" ~ expr) ^^ { case lhs ~ plus ~ rhs => BinaryOp("+", lhs, rhs) } |
      (term ~ "-" ~ expr) ^^ { case lhs ~ minus ~ rhs => BinaryOp("-", lhs, rhs) } |
      (term ~ "!" ~ "[+|-]".r ~ expr) ^^ { case lhs ~ factorial ~ sign ~ rhs => BinaryOp(sign, UnaryOp("!",lhs), rhs) } |
      (term ~ "[+|-]".r ~ expr ~ "!") ^^ { case lhs ~ sign ~ rhs ~ factor => BinaryOp(sign, lhs, UnaryOp("!", rhs)) } |
      ("sin(" ~>expr <~ ")") ^^ {  case x => UnaryOp("sin", x) } |
      ("cos(" ~>expr  <~ ")") ^^ {  case x => UnaryOp("cos", x) } |
      (term <~ "!") ^^ { case x => UnaryOp("!", x) } |
      ("-" ~> term) ^^ { case x => UnaryOp("-", x) } |
      term

  def term: Parser[Expr] =
    (factor ~ "^" ~ term) ^^ { case lhs ~ pow ~ rhs => BinaryOp("^", lhs, rhs) } |
    (factor ~ "!" ~ "[*|/|^]".r ~ factor) ^^ { case lhs ~ factorial ~ sign ~ rhs => BinaryOp(sign, UnaryOp("!",lhs), rhs) } |
      (factor ~ "[*|/|^]".r ~ factor ~ "!") ^^ { case lhs ~ sign ~ rhs ~ factorial => BinaryOp(sign, lhs, UnaryOp("!", rhs))} |
      (factor ~ "*" ~ term) ^^ { case lhs ~ times ~ rhs => BinaryOp("*", lhs, rhs) } |
      (factor ~ "/" ~ term) ^^ { case lhs ~ div ~ rhs => BinaryOp("/", lhs, rhs) } |
      ("-" ~> factor) ^^ { case x => UnaryOp("-", x) } |
      factor

  def factor: Parser[Expr] =
    ("-(" ~> expr <~ ")") ^^ { case x => UnaryOp("-", x) } |
      "(" ~> expr <~ ")" |
      "-" ~> expr |
      "(\\s*\\d+[,|.]\\d\\s*)|(\\s*\\d+\\s*)".r ^^ { x => Number(BigDecimal(x.replaceAll("""(?m)\s+$""","")))}

  def parse(text: String) = {
    parseAll(expr, text) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

