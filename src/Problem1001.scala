object Problem1001 {
  def main(args: Array[String]) {
    val numbersStrings = io.Source.stdin.getLines()
    val squares = numbersStrings.flatMap {
                    _.split(" ").filterNot { _.isEmpty }.map { x => math.sqrt(x.toLong) }
                  }.toList.reverse
    println(squares.mkString("\n"))
  }
}
