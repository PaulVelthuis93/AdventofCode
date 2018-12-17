import scala.io.Source

object Day12{

  private val initialStateRegex = """initial state: ([.#]+)""".r
  private val noteRegex = """([.#]{5}) => ([.#])""".r

  def parseNote(s: String): (String, String) = s match {
    case noteRegex(pattern, replace) => (pattern, replace)
  }

  def parseInput(input: List[String]): String = {
    val initialLine +: _ +: _ = input
    initialLine match {
      case initialStateRegex(initial) => initial // default value for example, where missing
    }
  }

  def parseNotes(input: List[String]): Map[String,String] = {
    input.drop(2)
      .map(s => s.split(" => "))
      .map{case Array(from, to) => from -> to }
      .toMap.withDefaultValue(".")
  }

  def calcSum(str: String, startIdx: Int = 0): Int =
    str.zipWithIndex.collect{case ('#', i) => i + startIdx}.sum

  def nextStep(str: String, idx: Int, notes: Map[String,String]) = {
    val temp = ("..." + str + "...")
      .sliding(5)
      .map(notes(_))
      .mkString("")
    val newIdx = idx - 1 + temp.indexOf('#')
    val newStr = temp.dropWhile(_ == '.')
    (newStr, newIdx)
  }

  def plantSum(input: List[String], generations: Int) ={
    val init = parseInput(input)
    val notes = parseNotes(input)
    val iter1 = Iterator.iterate((init, 0)){
      case (str, idx) => nextStep(str, idx, notes)
    }
    val (part1str, part1idx) = iter1.drop(generations).next
    calcSum(part1str, part1idx)
  }

  def plantSum(input: List[String], generations: Long) ={
    val init = parseInput(input)
    val notes = parseNotes(input)
    val iter2 = Iterator.iterate((1, init, 0, "")){
      case (step, str: String, idx, _) =>
        val (nextStr, nextIdx) = nextStep(str, idx, notes)
        (step + 1, nextStr, nextIdx, str)
    }
    val stable = iter2
      .dropWhile{case(_, cur, _, prev) => cur != prev}
      .map{case(step, cur, idx, _) => (step, idx, cur)}
    val (stableStep, stableIdx, stableStr: String) = stable.next
    val stableScore = calcSum(stableStr, stableIdx)
    val nextStableScore = calcSum(stableStr, stableIdx + 1) - nextStableScore
    stableScore + (generations - stableStep + 1) * nextStableScore
  }

  def main(args: Array[String]): Unit = {
    val generations: Int = 20
    val input = Source.fromFile("src/main/scala/files/inputday12.txt").getLines.toList
    println(plantSum(input, generations))
    val manyGenerations:Long = 50000000000L

    println(plantSum(input,manyGenerations))


  }
}
