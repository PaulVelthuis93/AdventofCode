import scala.annotation.tailrec

object Day14{

  val recipes = collection.mutable.ArrayBuffer[Int](3, 7)
  var elves = Vector(0, 1)

  @tailrec
  def numToDigits(n: Int, acc: List[Int] = Nil): List[Int] =
    n match {
      case 0 => if (acc == Nil) List(0) else acc
      case _ => numToDigits(n / 10, n % 10 :: acc)
    }

  def step() = {
    val recipeScore = elves.map(recipes(_)).sum
    recipes ++= numToDigits(recipeScore)
    elves = elves.map(e => (e + recipes(e) + 1) % recipes.size)
    recipeScore
  }

  def infiniteRecipes = new Iterator[Int] {
    var i = 0
    override def next = {
      while (i >= recipes.size) {
        step()
      }
      i += 1
      recipes(i - 1)
    }
    override def hasNext = true
  }

  def tenRecipeScore(input: Int): String = infiniteRecipes.slice(input, input + 10).mkString

  def recipeLeftOfScore(input: Int) = {
    val compare = numToDigits(input)
    infiniteRecipes.sliding(compare.size).zipWithIndex.collect {
      case (lst: Seq[Int], ind: Int) if lst == compare => ind
    }.next
  }

  def main(args: Array[String]): Unit = {
    val input= 430971
    println(tenRecipeScore(input))
    println(recipeLeftOfScore(input))
  }

}
