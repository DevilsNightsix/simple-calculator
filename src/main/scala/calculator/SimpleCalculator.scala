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

  def pow(x: BigDecimal, y: BigDecimal): BigDecimal = {
    var res = BigDecimal(1)
    var (lx, ly) = (x, y)
    while(ly !=0 ) {
      if (ly % BigDecimal(2) != 0) {
        res = res * lx
        ly =  ly -  1
      }
      lx = lx * lx;
      ly = ly / 2;
    }
    res
  }

  def eval(e: Expr): BigDecimal = {
    e match {
      case Number(x) => x
      case UnaryOp("-", x) => -(eval(x))
      case UnaryOp("!", x) => fact(eval(x))
      case UnaryOp("sin", x) => scala.math.sin((eval(x) % BigDecimal( 2 * math.Pi)).toDouble)
      case UnaryOp("cos", x) => scala.math.cos((eval(x) % BigDecimal(2 * math.Pi)).toDouble)
      case BinaryOp("+", x1, x2) => (eval(x1) + eval(x2))
      case BinaryOp("-", x1, x2) => (eval(x1) - eval(x2))
      case BinaryOp("/", x1, x2) => (eval(x1) / eval(x2))
      case BinaryOp("*", x1, x2) => (eval(x1) * eval(x2))
      case BinaryOp("^", x1, x2) => pow(eval(x1), eval(x2))
    }
  }
}

