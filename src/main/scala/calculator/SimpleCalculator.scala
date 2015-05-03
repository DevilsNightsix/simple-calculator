package calculator

class SimpleCalculator extends Calculator {
  override def compute(input: String): BigDecimal = {
    var code = MathParser.parse(input)
    eval(code)
  }

  def fact(x: BigDecimal): BigDecimal = {
    if(x <= 1)
      1
    else
      x * fact(x-1)
  }

  def eval(e: Expr): BigDecimal = {
    e match {
      case Number(x) => x
      case UnaryOp("-", x) => -(eval(x))
      case UnaryOp("!", x) => fact(eval(x))
      case UnaryOp("sin", x) => scala.math.sin(eval(x).toDouble)
      case UnaryOp("cos", x) => scala.math.cos(eval(x).toDouble)
      case BinaryOp("+", x1, x2) => (eval(x1) + eval(x2))
      case BinaryOp("-", x1, x2) => (eval(x1) - eval(x2))
      case BinaryOp("/", x1, x2) => (eval(x1) / eval(x2))
      case BinaryOp("*", x1, x2) => (eval(x1) * eval(x2))
      case BinaryOp("^", x1, x2) => scala.math.pow(eval(x1).toDouble, eval(x2).toDouble)
    }
  }
}

