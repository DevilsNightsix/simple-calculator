package calculator

object App {
  def main(args: Array[String]): Unit = {
    val s = new SimpleCalculator()
    println("Welcome to interactive Simple Calculator console (v 0.1)")
    val in: String = ""
    do {
      val in = readLine("|>")
      println(s.compute(in))
    } while(in != "exit")
  }
}

