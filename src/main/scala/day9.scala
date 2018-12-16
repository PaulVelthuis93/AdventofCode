import scala.io.Source

object Day9 {
  val regex = "(\\d+) players; last marble is worth (\\d+) points".r

  /**
    * A linked class where the current value is linked to a previous and a next value if there is one
    */
  class Linked[A](val value: A, var previous: Linked[A], var next: Linked[A])

  object Linked {
    /**
      * Setting the connection between objects, and returning this object
      */
    def apply[A](initial: A): Linked[A] = {
      val linked = new Linked(initial, null, null)
      linked.previous = linked
      linked.next = linked
      linked
    }
  }

  /**
    *
    * @param circle The circle consists of linked long values, because int is to low
    * @param marble The current marble number
    * @param player The current player number
    * @param scores the scores every round
    * @param last The last marble in the game
    * @param players amount of players
    * @return The winning elves his score
    */
  def game(circle: Linked[Long] = Linked(0), marble: Int = 1, player: Int = 0, scores: Map[Int, Long] = Map(), last: Int, players: Int): Long = {
    if(marble <= last) {
      // cases for marbles that have a rest when dividing by 23, otherwise something else happens
      if(marble % 23 != 0) {
        val (previous, next) = (circle, circle.next)
        // take the next one
        val insert = new Linked[Long](marble, previous, next)
        previous.next = insert
        next.previous = insert
        game(next, marble + 1, (player + 1) % players, scores, last, players)
      }
      // marble has not a rest number when dividing by 23
      else{
        val remove = (0 until 7 + 1).foldLeft(circle)((acc, _) => acc.previous)
        remove.previous.next = remove.next
        remove.next.previous = remove.previous

        game(remove.next.next, marble + 1, (player + 1) % players, scores + (player -> (scores.getOrElse(player, 0L) + remove.value + marble)), last, players)


      }
    }
    else
      scores.values.max
  }

  def main(args: Array[String]): Unit = {
    val (players, points) = Source.fromFile("src/main/scala/files/inputday9.txt").mkString.trim match {
      case regex(playersStr, pointsStr) => (playersStr.toInt, pointsStr.toInt)
    }
    println(game(last = points, players= players))
    println(game(last = points*100, players= players))
  }
}
