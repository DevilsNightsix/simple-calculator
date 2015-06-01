package calculator

import  scala.util.parsing.combinator._

object MathParser extends RegexParsers {
  val number =  "(\\s*\\d+[,|.]\\d\\s*)|(\\s*\\d+\\s*)".r
  val parantheses = "(" ~> exprBegin <~ ")"
  val varReg = "\\s*([aA-zZ])\\w*\\s*".r

  val getVariable = varReg ^^ {
    x => Variable(
        x.replaceAll("""(?m)\s+$""","")
      )
  }

  val bigDecimalNumber = number ^^ { 
    x => Number(
      BigDecimal(
        x.replaceAll("""(?m)\s+$""","")
      )
    )
 }

  
  def exprBegin: Parser[Expr] = varReg ~ "[=]".r ~ exprBegin ^^ {
    case lh ~ "=" ~ rh => BinaryOp("=", Variable(lh.replaceAll("""(?m)\s+$""","")), rh)
  } |
   multiplicantOp ~ rep("[+|-]".r ~ multiplicantOp) ^^ {
    case l ~ li => li.foldLeft(l) {
      case (lh, "+" ~ rh) => BinaryOp("+", lh, rh)
      case (lh, "-" ~ rh) => BinaryOp("-", lh, rh)
    }
  } 
 
  def multiplicantOp: Parser[Expr] = powOp ~ rep("[*|/]".r ~ powOp) ^^ {
    case l ~ li => li.foldLeft(l) {
      case(lh, "*" ~ rh) => BinaryOp("*", lh, rh)
      case(lh, "/" ~ rh) => BinaryOp("/", lh, rh)
    }
  }
 
  def powOp: Parser[Expr] = unaryOp ~ rep("[\\^]".r ~ unaryOp) ^^ {
    case l ~ li => li.foldLeft(l) {
      case(lh, "^" ~ rh) => BinaryOp("^", lh, rh)
  }}
 
  def unaryOp: Parser[Expr] = ("-" | "sin" | "cos") ~ suffixOp ^^ {
    case op ~ rh => op match {
      case "-"   => UnaryOp("-",   rh)
      case "sin" => UnaryOp("sin", rh)  
      case "cos" => UnaryOp("cos", rh)
    }
  } | suffixOp
 
  def suffixOp: Parser[Expr] = expr ~ "!" ^^ {
    case lh ~ op => op match {
      case "!" => UnaryOp("!", lh)
    }
  } | expr
 
  def expr: Parser[Expr] = getVariable | bigDecimalNumber | parantheses
  
  def parse(text: String) = {
    parseAll(exprBegin, text) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

