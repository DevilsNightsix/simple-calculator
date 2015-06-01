package calculator

abstract class Expr
case class Number(value: BigDecimal) extends Expr
case class Variable(name: String) extends Expr
case class UnaryOp(operator: String, arg: Expr) extends Expr
case class BinaryOp(operator: String, left: Expr, right: Expr) extends Expr

object Expr {
  implicit def expr2number(exr: Expr): Number = exr.asInstanceOf[Number]
  implicit def expr2variable(exr: Expr): Variable = exr.asInstanceOf[Variable]
  implicit def bigdecimal2Number(bd: BigDecimal) = Number(bd)
}
