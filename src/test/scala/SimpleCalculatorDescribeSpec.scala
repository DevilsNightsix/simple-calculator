package calculator

class SimpleCalculatorDescribeSpec extends DescribeSpec {
  val simpleCalculator = new SimpleCalculator()

  describe("Simple Calculator") {
    it("Should evaluate simple equation") {
      assert(simpleCalculator.compute("2 + 2") == 4.0)
    }

    it("Should assign variable") {
      simpleCalculator.compute("s = 2")
      assert(simpleCalculator.compute("s") == 2.0)
    }

    it("Should evaluate equations with variables") {
      simpleCalculator.compute("s = 2")
      assert(simpleCalculator.compute("s + 2") == 4.0)
    }
  }
}