
object Day18 {

  object FloydSolution {
    /**
      * https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
      */
    def floyd[A](x0: A, f: A => A): (Int, Int) = {
      var tortoise = f(x0)
      var hare = f(f(x0))
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(f(hare))
      }

      var μ = 0
      tortoise = x0
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(hare)
        μ += 1
      }

      var λ = 1
      hare = f(tortoise)
      while (tortoise != hare) {
        hare = f(hare)
        λ += 1
      }

      (μ, λ)
    }
  }

  case class Position(x: Int, y: Int) {
    def +(other: Position): Position = Position(x + other.x, y + other.y)
  }

  type Grid[A] = Vector[Vector[A]]

  /**
    * Grid consisting of positions
    *
    * @param grid grid to be made to position Grid
    * @tparam A type of Grid
    */
  implicit class PosGrid[A](grid: Vector[Vector[A]]) {
    def apply(pos: Position): A = grid(pos.y)(pos.x)
  }

  object Position {
    val axisOffsets: Seq[Position] = Seq(Position(0, 1), Position(-1, 0), Position(1, 0), Position(0, -1))
    val diagonalOffsets: Seq[Position] = Seq(Position(-1, 1), Position(1, 1), Position(-1, -1), Position(1, -1))
    val allOffsets: Seq[Position] = axisOffsets ++ diagonalOffsets
  }

  implicit class HeadIterator[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next) else None

    def head: A = headOption.get
  }

  implicit class GridOperator[A](grid: Grid[A]) {
    def containsPos(pos: Position): Boolean = grid.indices.contains(pos.y) && grid(pos.y).indices.contains(pos.x)

    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum
  }

  def step(grid: Grid[Char]): Grid[Char] = {
    (for ((row, y) <- grid.zipWithIndex.par)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Position(x, y)
          val neighbors = Position.allOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
          val trees = neighbors.count(_ == '|')
          val lumberyards = neighbors.count(_ == '#')
          cell match {
            case '.' if trees >= 3 => '|'
            case '|' if lumberyards >= 3 => '#'
            case '#' if trees >= 1 && lumberyards >= 1 => '#'
            case '#' => '.'
            case c => c
          }
        }).seq
  }

  def parseInput(input: String): Grid[Char] = input.lines.map(_.toVector).toVector

  def totalResourceValue(grid: Grid[Char], after: Int = 10): Int = {


    val it = Iterator.iterate(grid)(step)
    val finalGrid = it.drop(after).head
    val finalTrees = finalGrid.countGrid(_ == '|')
    val finalLumberyards = finalGrid.countGrid(_ == '#')
    finalTrees * finalLumberyards
  }

  def totalResourceValueBigNumber(grid: Grid[Char], after: Int = 1000000000): Int = {
    val (mu, lambda) = FloydSolution.floyd(grid, step)
    val afterMu = (after - mu) % lambda
    totalResourceValue(grid, mu + afterMu)
  }

  def main(args: Array[String]): Unit = {
    lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday18.txt")).mkString.trim
    val grid = parseInput(input)
    val minutes = 10
    println(totalResourceValue(grid,minutes))
    println(totalResourceValueBigNumber(grid))
  }

}