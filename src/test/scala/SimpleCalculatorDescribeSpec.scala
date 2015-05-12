package calculator

class SimpleCalculatorDescribeSpec extends DescribeSpec {
  val simpleCalculator = new SimpleCalculator()

  describe("Simple Calculator") {
    it("Should evaluate simple equation") {
      assert(simpleCalculator.compute("2 + 2") == 4.0)
    }
  }
}