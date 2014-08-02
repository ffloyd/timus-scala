import scala.annotation.tailrec

object Problem1002 {
  val stringOrder = Ordering.by { (_: String).size }

  def wordToNumbers(word: String): String = {
    word.map {
      case 'i' | 'j'        => '1'
      case 'a' | 'b' | 'c'  => '2'
      case 'd' | 'e' | 'f'  => '3'
      case 'g' | 'h'        => '4'
      case 'k' | 'l'        => '5'
      case 'm' | 'n'        => '6'
      case 'p' | 'r' | 's'  => '7'
      case 't' | 'u' | 'v'  => '8'
      case 'w' | 'x' | 'y'  => '9'
      case 'o' | 'q' | 'z'  => '0'
    }
  }

  def prepareDict(raw: List[String]): List[(String, String)] = raw.map { x => (x, wordToNumbers(x)) }

  def solve(number: String, dict: List[(String, String)]): Option[String] = {
    def generateCandidate(word: String, numericWord: String): Option[String] = {
      if ((number.size >= numericWord.size) && (number.substring(0, numericWord.size) == numericWord))
        if (number.size == numericWord.size)
          Some(word)
        else
          solve(number.substring(numericWord.size), dict) match {
            case Some(result: String) => Some(s"$word $result")
            case None => None
          }
      else
        None
    }

    dict.flatMap {
      case (word, numericWord) => generateCandidate(word, numericWord)
      case _ => None
    }.reduceOption(stringOrder.min)
  }

  @tailrec
  def processData() {
    val number: String = readLine()
    if (number == "-1")
      return

    val dictSize = readLine().toInt
    val rawDict = List.fill(dictSize) { readLine() }

    solve(number, prepareDict(rawDict)) match {
      case Some(result) => println(result)
      case None => println("No solution.")
    }
    processData()
  }

  def main(args: Array[String]) {
    processData()
  }
}
