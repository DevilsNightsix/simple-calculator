package calculator

class SimpleCalculator extends Calculator {
  override def compute(input: String): BigDecimal = {
    var code = MathParser.parse(input)
    BigDecimal(eval(code))
  }

  def fact(x: Double): Double = {
    if(x <= 1)
      1
    else
      x * fact(x-1)
  }

  def eval(e: Expr): Double = {
    simplify(e) match {
      case Number(x) => x
      case UnaryOp("-", x) => -(eval(x))
      case UnaryOp("!", x) => fact(eval(x))
      case UnaryOp("sin", x) => scala.math.sin(eval(x))
      case UnaryOp("cos", x) => scala.math.cos(eval(x))
      case BinaryOp("+", x1, x2) => (eval(x1) + eval(x2))
      case BinaryOp("-", x1, x2) => (eval(x1) - eval(x2))
      case BinaryOp("/", x1, x2) => (eval(x1) / eval(x2))
      case BinaryOp("*", x1, x2) => (eval(x1) * eval(x2))
      case BinaryOp("^", x1, x2) => scala.math.pow(eval(x1), eval(x2))
    }
  }

  def simplify(e: Expr): Expr = {
    e match {
      case UnaryOp("-", UnaryOp("-", x)) => x
      case UnaryOp("+", x) => x
      case BinaryOp("*", x, Number(1)) => x
      case BinaryOp("*", Number(1), x) => x
      case BinaryOp("*", x, Number(0)) => Number(0)
      case BinaryOp("*", Number(0), x) => Number(0)
      case BinaryOp("/", x, Number(1)) => x
      case BinaryOp("+", x, Number(0)) => x
      case BinaryOp("+", Number(0), x) => x
      case BinaryOp("-", x, Number(0)) => x
      case _ => e
    }
  }
}

