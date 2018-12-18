import scala.util.matching.Regex

object Day17 {

  val regex: Regex = """(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)""".r

  case class Position(x: Int, y: Int) {
    def +(other: Position): Position = Position(x + other.x, y + other.y)
  }

  sealed trait Tile

  case object Sand extends Tile {
    override def toString: String = "."
  }

  case object Clay extends Tile {
    override def toString: String = "#"
  }

  type Tiles = Map[Position, Tile]

  trait WaterTile extends Tile

  case object BecameDry extends WaterTile {
    override def toString: String = "|"
  }

  /**
    * water can reach this tile, but it can not stop here
    */
  case object HasToMove extends WaterTile {
    override def toString: String = "\\"
  }

  case object Settled extends WaterTile {
    override def toString: String = "~"
  }

  def boundariesRect(positions: Seq[Position]): (Position, Position) = {
    (Position(positions.minBy(_.x).x, positions.minBy(_.y).y), Position(positions.maxBy(_.x).x, positions.maxBy(_.y).y))
  }

  def settle(tiles: Tiles, pos: Position): Tiles = {
    tiles(pos) match {
      case HasToMove =>
        settle(settle(tiles + (pos -> Settled), pos + Position(-1, 0)), pos + Position(1, 0))
      case _ => tiles
    }
  }

  def flood(tiles: Tiles, maxY: Int, pos: Position, prevPos: Position): Tiles = {
    //the water always moves down
    if (pos.y > maxY)
      tiles
    else {
      tiles(pos) match {
        case Clay | BecameDry | Settled | HasToMove => tiles
        case Sand =>
          //every time move one position down
          val downPos = pos + Position(0, 1)
          val downTiles = flood(tiles + (pos -> BecameDry), maxY, downPos, pos)
          downTiles(downPos) match {
            case BecameDry | HasToMove => downTiles
            case Clay | Settled =>
              val leftPos = pos + Position(-1, 0)
              val leftTiles = flood(downTiles, maxY, leftPos, pos)
              val rightPos = pos + Position(1, 0)
              val rightTiles = flood(leftTiles, maxY, rightPos, pos)
              (rightTiles(leftPos), rightTiles(rightPos)) match {
                case (Clay | Settled | HasToMove, _) | (_, Clay | Settled | HasToMove) if prevPos == leftPos || prevPos == rightPos =>
                  rightTiles + (pos -> HasToMove)
                case (Clay | Settled | HasToMove, Clay | Settled | HasToMove) =>
                  settle(settle(rightTiles, leftPos), rightPos) + (pos -> Settled)
                case _ => rightTiles
              }
            case Sand => downTiles
          }
      }

    }
  }

  def waterReachOverTilesInRange(input: String): Int = {
    val tiles = parseInput(input)
    val (min, max) = boundariesRect(tiles.keys.toSeq)
    val flooded = flood(tiles, max.y, Position(500, 0), Position(500, -1))
    flooded.count({ case (pos, tile) => pos.y >= min.y && tile.isInstanceOf[WaterTile] })


  }

  def waterTilesLeftAfterSpringDrought(input: String): Int = {
    val tiles = parseInput(input)
    val (min, max) = boundariesRect(tiles.keys.toSeq)
    val flooded = flood(tiles, max.y, Position(500, 0), Position(500, -1))
    flooded.count({ case (pos, tile) => pos.y >= min.y && tile == Settled})
  }

  def parseLine(line: String): Tiles = line match {
    case regex("x", x, "y", y1, y2) => (y1.toInt to y2.toInt).map(Position(x.toInt, _) -> Clay).toMap
    case regex("y", y, "x", x1, x2) => (x1.toInt to x2.toInt).map(Position(_, y.toInt) -> Clay).toMap
  }

  def parseInput(input: String): Tiles = {
    input.lines.map(parseLine).reduce(_ ++ _).withDefaultValue(Sand)
  }

  def main(args: Array[String]): Unit = {
    lazy val input = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday17.txt")).mkString.trim
    println(waterReachOverTilesInRange(input))
    println(waterTilesLeftAfterSpringDrought(input))
  }
}
