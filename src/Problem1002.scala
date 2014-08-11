import scala.annotation.tailrec
import scala.collection.{mutable, immutable}

case class DictItem(word: String, number: String)

case class PartialSolution(position: Int, words: List[String])

class Dictionary(source: List[String]) {
  val items: List[DictItem] = distinctByNumber(source.map(genDictItem))

  private def genDictItem(word: String): DictItem = {
    DictItem(
      word,
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
        case _ => throw sys.error("Unacceptable input")
      }
    )
  }

  private def distinctByNumber(list: List[DictItem]): List[DictItem] = {
    val seen = mutable.HashSet[String]()
    list.flatMap { x =>
      if (seen(x.number)) {
        None
      } else {
        seen += x.number
        Some(x)
      }
    }
  }
}

object Problem1002 {
  
  sealed abstract class ProcessingResult()
  case class Answer(strings: List[String]) extends ProcessingResult
  case class NewPartialSolutions(solutions: List[PartialSolution]) extends ProcessingResult

  def solve(number: String, dict: Dictionary): String = {
    val seenPosition  = new mutable.HashSet[Int]

    def processPartialSolution(ps: PartialSolution): ProcessingResult = {
      @tailrec
      def recursiveImpl(acc: List[PartialSolution], lastItems: List[DictItem]): ProcessingResult =
        lastItems match {
          case Nil    => NewPartialSolutions(acc)
          case items  =>
            val item          = items.head
            val newPosition   = ps.position + item.number.length
            val matched       = number.startsWith(item.number, ps.position) && !seenPosition(newPosition)
            val thisIsAnswer  = matched && (newPosition == number.length)

            if (thisIsAnswer)
              Answer((item.word :: ps.words).reverse)
            else if (matched) {
              val newPS = PartialSolution(newPosition, item.word :: ps.words)
              seenPosition += newPosition
              recursiveImpl(newPS :: acc, items.tail)
            } else
              recursiveImpl(acc, items.tail)
        }

      recursiveImpl(Nil, dict.items)
    }

    @tailrec
    def bfs(state: immutable.Queue[PartialSolution]): List[String] = if (state.isEmpty)
      Nil
    else {
      val (current, rest) = state.dequeue
      processPartialSolution(current) match {
        case Answer(strings)                => strings
        case NewPartialSolutions(solutions) => bfs(rest enqueue solutions)
      }
    }

    bfs(immutable.Queue(PartialSolution(0, Nil))) match {
      case Nil    => "No solution."
      case answer => answer.mkString(" ")
    }
  }

  @tailrec
  def processData() {
    val number: String = readLine()
    if (number == "-1")
      return

    val dictSize = readLine().toInt
    val rawDict = List.fill(dictSize) { readLine() }

    println(solve(number, new Dictionary(rawDict)))

    processData()
  }

  def main(args: Array[String]) {
    processData()
  }
}