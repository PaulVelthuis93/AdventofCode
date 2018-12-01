import scala.collection.mutable

object Day1 {

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday1.txt")).mkString.trim
  lazy val frequencies: Seq[Int] = input.lines.map(_.toInt).toSeq

  def resultingFreq(freqChanges: Seq[Int]): Int = freqChanges.sum


  /**
    * find the first frequency that occurs twice
    *
    * @param freqChanges
    * @return the frequency that is found to be occuring twice, or none if it is not found
    */
  def firstTwiceFrequency(freqChanges: Seq[Int]): Option[Int] = {

    val it = Iterator.continually(freqChanges).flatten
    val runningtotal = it.scanLeft(0)(_ + _)
    val prevFreqs = mutable.Set[Int]()
    var firstTwice: Option[Int] = None
    do {
      val freq = runningtotal.next()
      //      add returns false if element already in set
      if (!prevFreqs.add(freq))
        firstTwice = Some(freq)
    } while (firstTwice.isEmpty)
    firstTwice

  }

  def main(args: Array[String]): Unit = {
    println(resultingFreq(frequencies))
    println(firstTwiceFrequency(frequencies))
  }
}

