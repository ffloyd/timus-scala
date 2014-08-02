import scala.annotation.tailrec

object Problem1002 {
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

  def solve(number: String, dict: List[(String, String)]): List[String] = {
    def numberBeginsWith(numericWord: String): Boolean =
      (number.size >= numericWord.size) &&
        (number.substring(0, numericWord.size) == numericWord)

    def generateCandidate(dictEntry: (String, String)): List[String] = {
      val (word, numericWord) = dictEntry

      if (numberBeginsWith(numericWord))
        if (number.size == numericWord.size)
          List(word)
        else
          solve(number.substring(numericWord.size), dict) match {
            case Nil    => Nil
            case result => word :: result
          }
      else
        Nil
    }

    def cmpF(arg: List[String]): Int = {
      arg.size match {
        case 0   => Int.MaxValue
        case any => any
      }
    }

    dict.map(generateCandidate).minBy(cmpF)
  }

  @tailrec
  def processData() {
    val number: String = readLine()
    if (number == "-1")
      return

    val dictSize = readLine().toInt
    val rawDict = List.fill(dictSize) { readLine() }

    solve(number, prepareDict(rawDict)) match {
      case Nil      => println("No solution.")
      case result   => println(result.mkString(" "))
    }

    processData()
  }

  def main(args: Array[String]) {
    processData()
  }
}
