import scala.collection.mutable
import scala.io.Source

object Day3 {

  val matrix: Array[Array[Int]] = Array.ofDim[Int](1000, 1000)

  def initialize() = {
    val input = Source.fromFile("src/main/scala/files/inputday3.txt").getLines
    for (line <- input) {
      // replace the double dot to make coordinate split correct
      val claim = line.replace(":", "").split(" ")
      val coord = claim(2).split(",")
      val dimension = claim(3).split("x")
      // all the x dimensions, we go from the starting coordinate until the coordinate + the x dimension
      for (x: Int <- coord(0).toInt until coord(0).toInt + dimension(0).toInt) {
        // all the y dimensions, we go from the starting coordinate until the coordinate + the y dimension
        for (y: Int <- coord(1).toInt until coord(1).toInt + dimension(1).toInt) {
          // every place occupied we add one, so if two people want to have it the matrix value is 2
          matrix(x)(y) += 1
        }
      }
    }
  }

  /**
    * find the elfId that can make the suit, because it has the fabric with no claims
    * we loop once more over the input list
    * This loop is required to verify if
    * @return the elfId that has the fabric without claims make the suit, or 0 if no elf has a fabric that is unclaimed by others
    */
    def findElfId(): Int = {
      val input = Source.fromFile("src/main/scala/files/inputday3.txt").getLines
      for (line <- input) {
        var intact = true
        // replace the double dot to make coordinate split correct
        val claim = line.replace(":", "").split(" ")
        val elfId: Int = claim(0).replace("#", "").toInt
        val coord = claim(2).split(",")
        val dimension = claim(3).split("x")
        // all the x dimensions, we go from the starting coordinate until the coordinate + the x dimension
        for (x: Int <- coord(0).toInt until coord(0).toInt + dimension(0).toInt) {
          // all the y dimensions, we go from the starting coordinate until the coordinate + the y dimension
          for (y: Int <- coord(1).toInt until coord(1).toInt + dimension(1).toInt) {
            if (matrix(x)(y) >= 2) {
              intact = false
            }
          }
        }
        if (intact){
          return elfId
        }
      }
      0
    }

  def findDuplicate(): Int = {
    // find the duplicates
    var duplicate: Int = 0
    for (i: Int <- 0 until 1000) {
      for (j: Int <- 0 until 1000) {
        if (matrix(i)(j) > 1)
          duplicate += 1
      }
    }
    duplicate
  }

  def main(args: Array[String]): Unit = {
    initialize()
    println(findDuplicate())
    println(findElfId())
  }
}