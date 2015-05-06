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
    assert(simpleCalculator.compute("2 + (3 - 1)") == BigDecimal(4.0))
    assert(simpleCalculator.compute("2 + (3 * 2 - 5)") == BigDecimal(3.0))
  }

  "A Simple calculator" should "evaluate factorial of a number" in {
    assert(simpleCalculator.compute("5!") == BigDecimal(120.0))
  }

  "A simple calculator" should "evaluate involution of number" in {
    assert(simpleCalculator.compute("2^10") == BigDecimal(1024.0))
  }

  "A simple calculator" should "evaluate sin and cos" in {
    assert(simpleCalculator.compute("sin(1)") == BigDecimal(math.sin(1)))
    assert(simpleCalculator.compute("cos(1)") == BigDecimal(math.cos(1)))
  }

  "A simple calculator" should "evaluate complex expressions" in {
    simpleCalculator.compute("-(23 + 1 / 4) * 2! + sin(cos(1 + 2 ^ 4))").toDouble shouldEqual  BigDecimal(-46.77170413398965854).toDouble
  }

  "A Simple calculator" should "evaluate big numbers" in {
    assert(
    simpleCalculator.compute("10000000000000000000000000000000000000000000000000 - 1") ==
    BigDecimal("10000000000000000000000000000000000000000000000000") - BigDecimal(1)
    )
  }

  "A simple calculator" should "evaluate complex exprs" in {
    assert(simpleCalculator.compute("-(4 + 4) / (9 + - 5/(23 - 22))") == BigDecimal(-2))
  }
}
