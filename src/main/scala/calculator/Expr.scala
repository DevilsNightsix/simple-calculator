package calculator

abstract class Expr
case class Number(value: Double) extends Expr
case class UnaryOp(operator: String, arg: Expr) extends Expr
case class BinaryOp(operator: String, left: Expr, right: Expr) extends Expr
