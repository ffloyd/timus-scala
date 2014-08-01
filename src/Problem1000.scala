object Problem1000 {
  def main(args: Array[String]) = {
    val numbersString = readLine()
    println {
      val Array(a, b) = for { x <- numbersString.split(" ") } yield x.toInt
      a + b
    }
  }
}