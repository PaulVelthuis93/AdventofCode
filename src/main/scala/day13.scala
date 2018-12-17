object Day13{

  case class Position(x: Int, y: Int) {
    def +(other: Position) = Position(x + other.x, y + other.y)
  }

  class Field(field: IndexedSeq[IndexedSeq[Char]]) {
    def apply(pos: Position) = field(pos.y)(pos.x)
  }

  case class Cart(position: Position, direction: Position, turnStatus: Int = 0) extends Ordered[Cart] {
    def tick(field: Field) = {
      val nextPosition = position + direction
      val nextTrack = field(nextPosition) ensuring (_ != ' ')
      val nextDirection = nextTrack match {
        case '\\' => Position(direction.y, direction.x)
        case '/' => Position(-direction.y, -direction.x)
        case '+' => turnStatus match {
          case 0 => Position(direction.y, -direction.x)
          case 1 => direction
          case 2 => Position(-direction.y, direction.x)
        }
        case _ => direction
      }
      val nextTurnStatus =
        if (nextTrack == '+') (turnStatus + 1) % 3
        else turnStatus
      Cart(nextPosition, nextDirection, nextTurnStatus)
    }

    override def compare(other: Cart) =
      if (position.y == other.position.y) position.x - other.position.x
      else position.y - other.position.y

  }

  def findCollisions(positions: Seq[Position]) = positions.diff(positions.distinct)

  def cartSimulate(removeCollisions: Boolean = false) = {
    val (field, initialCarts) = {
      val chrToDir = Map(
        '<' -> ('-', Position(-1, 0)),
        '>' -> ('-', Position(1, 0)),
        '^' -> ('|', Position(0, -1)),
        'v' -> ('|', Position(0, 1))
      )
      val (field, carts) = scala.io.Source.fromFile("src/main/scala/files/inputday13.txt").getLines.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (chr, x) =>
          chrToDir.get(chr) match {
            case Some((track, dir)) => (track, Some(Cart(Position(x, y), dir)))
            case None => (chr, None)
          }
        }.unzip
      }.toVector.unzip

      (new Field(field), carts.flatten.flatten)
    }
    Stream.iterate(Vector(initialCarts)) { carts =>
      var cartsByTurn = carts.last.sorted
      val subticks = new scala.collection.mutable.ArrayBuffer[Vector[Cart]]

      var i = 0
      while (i < cartsByTurn.size) {
        cartsByTurn = cartsByTurn.updated(i, cartsByTurn(i).tick(field))
        if (removeCollisions) {
          val collisions = findCollisions(cartsByTurn.map(_.position))
          i -= cartsByTurn.zipWithIndex.collect { case (c, ind) if collisions.contains(c.position) => ind }.count(_ <= i)
          cartsByTurn = cartsByTurn.filter { c => !collisions.contains(c.position) }
        }
        subticks += cartsByTurn
        i += 1
      }
      subticks.toVector
    }.flatten
  }

  def cartCollision(): Position ={
    cartSimulate().map(c => findCollisions(c.map(_.position))).filter(_.nonEmpty).head.head
  }

  def lastCart(): Position ={
    cartSimulate(true).filter{_.size == 1}(1).head.position
  }

  def main(args: Array[String]): Unit = {
    println(cartCollision())
    println(lastCart())
  }
}
