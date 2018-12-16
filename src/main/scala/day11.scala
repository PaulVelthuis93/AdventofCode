object Day11 {

  val input = 5535
  val size = 300
  val powers: IndexedSeq[IndexedSeq[Int]] = (0 until size).map(y => (0 until size).map(x => powerLevel(x, y)))

  def powerLevel(x: Int, y: Int): Int = {
    val rackId = x + 10
    val power = (rackId * y + input) * rackId
    power.toString.reverse(2).asDigit - 5
  }

  def maxSquareSubArray(k: Int): (Int, Int, Int, Int) = {
    val matrix = Array.ofDim[Int](size, size)
    for(j <- 0 until size) {
      var sum = (0 until k).map(powers(_)(j)).sum
      matrix(0)(j) = sum
      for(i <- 1 until size - k + 1) {
        sum += powers(i + k - 1)(j) - powers(i - 1)(j)
        matrix(i)(j) = sum
      }
    }
    var maxSum = Int.MinValue
    var (mx, my) = (0, 0)
    for(i <- 0 until size) {
      var sum = (0 until k).map(matrix(i)(_)).sum
      if(sum >= maxSum) {
        maxSum = sum
        mx = 0
        my = i
      }
      for(j <- 1 until size - k + 1) {
        sum += matrix(i)(j + k - 1) - matrix(i)(j - 1)
        if(sum > maxSum) {
          maxSum = sum
          mx = j
          my = i
        }
      }
    }
    (mx, my, k, maxSum)
  }

  def main(args: Array[String]): Unit = {
    val (x, y, _, _) = maxSquareSubArray(3)
    println("x:" + x, " y:" + y)
    val (x2, y2, k, _) = (1 until size).map(k => maxSquareSubArray(k)).maxBy(_._4)
    println("part 2, x:" + x2, " y:" + y2, " dimension:" + k)
  }
}
