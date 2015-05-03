package calculator

class SimpleCalculatorSpec extends UnitSpec {

  val simpleCalculator: SimpleCalculator = new SimpleCalculator()

  "A Simple calculator" should "evaluate simple expressions" in {
    assert(simpleCalculator.compute("2 + 2") == 4.0)
    assert(simpleCalculator.compute("2 * 2") == 4.0)
    assert(simpleCalculator.compute("2 - 2") == 0.0)
    assert(simpleCalculator.compute("2 / 2") == 1.0)
  }

  "A Simple calculator" should "evaluate expression in parenthesis" in {
    assert(simpleCalculator.compute("2 + (3 - 1)") == 4.0)
    assert(simpleCalculator.compute("2 + (3 * 2 - 5)") == 3.0)
  }

  "A Simple calculator" should "evaluate factorial of a number" in {
    assert(simpleCalculator.compute("5!") == 120.0)
  }

  "A simple calculator" should "evaluate involution of number" in {
    assert(simpleCalculator.compute("2^10") == 1024.0)
  }

  "A simple calculator" should "evaluate sin and cos" in {
    assert(simpleCalculator.compute("sin(1)") == BigDecimal(math.sin(1)))
    assert(simpleCalculator.compute("cos(1)") == BigDecimal(math.cos(1)))
  }

  "A simple calculator" should "evaluate complex expressions" in {
    val res = -(23 + 1.0/4.0) * 2 + math.sin(math.cos(1 + math.pow(2, 4)))
    assert(simpleCalculator.compute("-(23 + 1 / 4) * 2! + sin(cos(1 + 2 ^ 4))") == BigDecimal(res))
  }
}
