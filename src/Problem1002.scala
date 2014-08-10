import scala.annotation.tailrec
import scala.collection.mutable

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
      }
    )
  }

  private def distinctByNumber(list: List[DictItem]): List[DictItem] = {
    val seen = mutable.HashSet[String]()
    list.flatMap { x =>
      if (seen(x.number))
        None
      else {
        seen += x.number
        Some(x)
      }
    }
  }
}

object Problem1002 {

  def solve(number: String, dict: Dictionary): String = {
    type State    = List[PartialSolution]
    type Solution = List[String]

    val positionSeen = mutable.HashSet[Int]()

    @tailrec
    def producePartialState(ps: PartialSolution, init: State = Nil, items: List[DictItem] = dict.items)
    : Either[State, Solution] = {
      items match {
        case Nil => Left(init)
        case _   =>
          val item = items.head
          if (number.startsWith(item.number, ps.position)) {
            if (ps.position + item.number.size == number.size)
              Right((item.word :: ps.words).reverse)
            else {
              val newPosition = ps.position + item.number.length
              if (positionSeen(newPosition)) {
                producePartialState(ps, init, items.tail)
              }
              else {
                positionSeen += newPosition
                val generated = PartialSolution(newPosition, item.word :: ps.words)
                producePartialState(ps, generated :: init, items.tail)
              }
            }
          }
          else
            producePartialState(ps, init, items.tail)
      }
    }

    @tailrec
    def produceNextState(state: State, init: State = Nil): Either[State, Solution] = state match {
      case Nil => Left(init)
      case _   =>
        producePartialState(state.head) match {
          case Left(partialState) => produceNextState(state.tail, partialState ::: init)
          case Right(solution)    => Right(solution)
        }
    }

    def BFS(state: State): Solution = {
      state match {
        case Nil => Nil
        case _ =>
          produceNextState(state) match {
            case Left(newState)   => BFS(newState)
            case Right(solution)  => solution
          }
      }
    }

    val initialState: State = List(PartialSolution(0, Nil))
    BFS(initialState) match {
      case Nil => "No solution."
      case any => any.mkString(" ")
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