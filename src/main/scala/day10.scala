import scala.io.Source

object Day10 {

  val regex = "position=< *?(-?\\d+), +(-?\\d+)> velocity=< *?(-?\\d+), +(-?\\d+)>".r

  case class Dot(positionX: Int, positionY: Int, velocityX: Int, velocityY: Int)

  /**
    * @return the message to be displayed,
    *         and the time that would normally be waited to see te message appear
    */
  def message(dots: Seq[Dot], time: Int): (String, Int) = {
    val set = dots.map(p => (p.positionX, p.positionY))
    val aligned: Seq[(Int, Int)] = for {
      (x, y) <- set
      if (1 until 10).forall(i => set.contains((x, y + i)))
    } yield (x, y)
    if (aligned.nonEmpty) {
      val (minX, maxX, minY, maxY) = (dots.map(_.positionX).min, dots.map(_.positionX).max, dots.map(_.positionY).min, dots.map(_.positionY).max)
      val string = (minY to maxY).map(y => (minX to maxX).map(x => if (set.exists { case (px, py) => px == x && py == y }) "#" else ".").mkString).mkString("\n")
      (string, time)
    } else {
      message(dots.map(p => p.copy(positionX = p.positionX + p.velocityX, positionY = p.positionY + p.velocityY)), time + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val movingDots = Source.fromFile("src/main/scala/files/inputday10.txt").getLines.map {
      case regex(posX, posY, velX, velY) => Dot(posX.toInt, posY.toInt, velX.toInt, velY.toInt)
    }.toSeq
    println(message(movingDots, 0))

  }
}
