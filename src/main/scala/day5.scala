
object Day5 {

  def opposites(a: Char, b: Char): Boolean = a != b && a.toLower == b.toLower

  def polymerReaction(s: String): String= {
    s.foldLeft(List[Char]())({
      //removes all the oposites if the chars next each other are opposites
      case (a :: it, b) if opposites(a,b) => it
       // add the element at the beginning
      case (charList, b) => b :: charList
    }).reverse.mkString("")
  }

  def polymerReactionLength(s: String): Int = polymerReaction(s: String).length

  def shortestPolymer(s: String)={
  val reaction = polymerReaction(s: String)
  val reactionLowerCase = reaction.toLowerCase.toSet
  reactionLowerCase.map(lowerChar => reaction.filterNot(_.toLower == lowerChar)).map(polymerReactionLength).min
}

  def main(args: Array[String]): Unit = {
    lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday5.txt")).mkString.trim
    println(polymerReactionLength(input))
    println(shortestPolymer(input))
  }


}