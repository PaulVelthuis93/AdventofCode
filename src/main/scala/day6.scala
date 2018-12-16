import scala.io.Source

object Day6 {

  case class Point(x: Int, y: Int) {
    def distanceTo(coordinate: Coordinate): Int = {
      (x - coordinate.x).abs + (y - coordinate.y).abs
    }
  }

  case class Coordinate(x: Int, y: Int) {

    def findClosestPoint(points: List[Point]): Option[Point] = {
      val distances: Seq[(Int, Point)] = points.map(point => point.distanceTo(this) -> point)
      val minPointDistance = distances.minBy(_._1)
      if (distances.count(_._1 == minPointDistance._1) == 1) {
        return Some(minPointDistance._2)
      }
      None
    }

    def summedDistanceTo(points: List[Point]): Int = {
      points.foldLeft(0)(_ + _.distanceTo(this))
    }
  }

  /**
    * for every coordinate, go over all the points and find the closest.
    * @param coordinates
    * @param points
    * @return
    */
  private def calculateClosestPoints(coordinates: List[Coordinate], points: List[Point]): Map[Coordinate, Point] = {
    coordinates
      .map(coordinate => coordinate -> coordinate.findClosestPoint(points))
      .toMap
      .filter(_._2.isDefined)
      .mapValues(_.get)
  }

  /**
    * a sorted list with coordinates
    * @param points
    * @return a sorted list with coordinates
    */
  private def createCoordinates(points: List[Point]): List[Coordinate] = {
    val x = points.map(_.x)
    val y = points.map(_.y)

    (
      for {
      x <- Stream.from(x.min).take(x.max)
      y <- Stream.from(y.min).take(y.max)
    } yield Coordinate(x, y)).toList
  }

  private def findInfinitePoints(distancesToPoints: Map[Coordinate, Point], points: List[Point]): Set[Point] = {
    val x = points.map(_.x)
    val y = points.map(_.y)

    distancesToPoints
      .filterKeys(coordinate => coordinate.x == x.min || coordinate.x == x.max || coordinate.y == y.min || coordinate.y == y.max)
      .values
      .toSet
  }

  private def countCoordinatesClosestToPoints(distancesToPoints: Map[Coordinate, Point], infinitePoints: Set[Point]): Map[Point, Int] = {
    distancesToPoints
      .values
      .filterNot(infinitePoints.contains)
      .groupBy(identity)
      .mapValues(_.size)
  }

  def closestPointNotInfinite(points: List[Point], coordinates: List[Coordinate])={

    println(coordinates.head)
    val closestPoints = calculateClosestPoints(coordinates, points)
    val infinitePoints = findInfinitePoints(closestPoints, points)
    val closestPointsCount = countCoordinatesClosestToPoints(closestPoints, infinitePoints)
    closestPointsCount.maxBy(_._2)._2
  }

  def RegionSizeAllLocationsByLimit(coordinates: List[Coordinate], points: List[Point], limit: Int): Int = {
    coordinates.count(_.summedDistanceTo(points) < limit)
  }

  def main(args: Array[String]): Unit = {
    val coordinatesRegex = s"""(.*), (.*)""".r
    lazy val input = Source.fromFile("src/main/scala/files/inputday6.txt").getLines.map{
      case coordinatesRegex(x, y) => Point(x.toInt, y.toInt)
    }.toList
    val coordinates: List[Coordinate] = createCoordinates(input)
    println(closestPointNotInfinite(input, coordinates))
    println(RegionSizeAllLocationsByLimit( coordinates, input, 10000))

  }

}