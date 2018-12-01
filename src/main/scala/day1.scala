object Day1 {

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday1.txt")).mkString.trim
  lazy val frequencies: Seq[Int] = input.lines.map(_.toInt).toSeq

  def resultingFreq(freqChanges: Seq[Int]): Int = freqChanges.sum

  def main(args: Array[String]): Unit = {
    println(resultingFreq(frequencies))
  }
}

