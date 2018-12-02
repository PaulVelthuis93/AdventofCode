import scala.io.Source

object Day2 {

  def counting(): Int = {
    lazy val input = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday2.txt"))
    var count2: Int = 0
    var count3: Int = 0

    for (line <- input.getLines()) {
      val mappie = line.groupBy(identity).mapValues(_.length)
      if (mappie.values.exists(_ == 2)) {
        count2 += 1
      }
      if (mappie.values.exists(_ == 3)) {
        count3 += 1
      }
    }
    count2 * count3
  }


  /**
    * check if the elements of the first string match with the last string
    *map all the occurring elements in both, so drop the other characters
    * @param first first string to be compared
    * @param last second string to be compared
    * @return string with similar characters found in both strings
    */
  def createOccuringString(first: String, last:String) ={
    first.zipWithIndex.filter(a => a._1 == last(a._2))
      .map(_._1).foldLeft("")(_+_)
  }

  /**
    * find the correct box id, and give it as a result
    * @return return the most correct string with the dropped differing characters
    */
  def mostSimilar(): String = {
    val inputList = Source.fromFile("src/main/scala/files/inputday2.txt").getLines.toList
    inputList.combinations(2)
      .map(tuple => {
        (tuple.head.length - 1, createOccuringString(tuple.head, tuple.last))
      })
      .find(a => a._1 == a._2.length)
      // see if a string with same occurances is found
      .map(_._2)
      .getOrElse("")
    }

  def main(args: Array[String]): Unit = {
    println(counting())
    println(mostSimilar())
  }

}
