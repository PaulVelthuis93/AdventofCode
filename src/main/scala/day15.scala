import scala.util.Try

object Day15{


  type Grid[A] = Vector[Vector[A]]

  implicit class PosGrid[A](grid: Vector[Vector[A]]) {
    def apply(pos: Position): A = grid(pos.y)(pos.x)
  }

  sealed trait UnitType {
    def target: UnitType
  }

  case class Position(x: Int, y: Int){
    def +(other: Position): Position = Position(x + other.x, y + other.y)
  }

  object Position {
    val axisOffsets: Seq[Position] = Seq(Position(0, 1), Position(-1, 0), Position(1, 0), Position(0, -1))
  }

  case object Elf extends UnitType {
    override def target: UnitType = Goblin
  }

  case object Goblin extends UnitType {
    override def target: UnitType = Elf
  }

  /** This orders the position of the units
    *
    */
  implicit val posReadingOrdering: Ordering[Position] = Ordering.fromLessThan({ (pos1, pos2) =>
    pos1.y < pos2.y || (pos1.y == pos2.y && pos1.x < pos2.x)
  })

  /**
    * In this game the postion decides who fights first
    */
  implicit val combatUnitReadingOrdering: Ordering[CombatUnit] = Ordering.by(_.pos)

  /**
    * In the basic game each combat unit has a type, postion 200 HP and attackpower
    * @param unitType type of the unit
    * @param pos position of the unit
    * @param hp the hp default is 200
    * @param attackPower The attack power default is 3
    */
  case class CombatUnit(unitType: UnitType, pos: Position, hp: Int = 200, attackPower: Int = 3)

  /**
    * retuns the set with units if it is a target
    * @param unit the unit for which a target unit is searched
    * @param units list of target units for which a target is searched
    * @return Set with target units
    */
  def getTargets(unit: CombatUnit)(implicit units: List[CombatUnit]): Set[CombatUnit] = units.filter(_.unitType == unit.unitType.target).toSet

  /**
    * says whether a position is free to move to
    */
  def isFree(pos: Position)(implicit grid: Grid[Char], units: List[CombatUnit]): Boolean = {
    grid(pos) == '.' && !units.exists(_.pos == pos)
  }

  /**
    * try to get in range of a target unit
    * @param targets the target units
    * @param grid the map
    * @param units the list of units
    * @return set of positions to move to
    */
  def getInRange(targets: Set[CombatUnit])(implicit grid: Grid[Char], units: List[CombatUnit]): Set[Position] = {
    for {
      target <- targets
      offset: Position <- Position.axisOffsets
      pos: Position = target.pos + offset
      if isFree(pos)
    } yield pos
  }

  /**
    * Breadth-first search (BFS) is an algorithm for traversing or searching tree or graph data structures.
    * It starts at the tree root,
    * and explores all of the neighbor nodes at the present depth prior to moving on to the nodes at the next depth level.
    * @param startPos root position to search from
    * @param endPos end position
    * @param grid the positon grid
    * @param units the units
    * @return Map with Positions and amount of units
    */
  def bfs(startPos: Position, endPos: Set[Position])(implicit grid: Grid[Char], units: List[CombatUnit]): Map[Position, Int] = {

    def helper(visited: Map[Position, Int], toVisit: Map[Position, Int]): Map[Position, Int] = {
      val neighbors = for  {
        (pos, dist) <- toVisit
        offset <- Position.axisOffsets
        newPos = pos + offset
        if isFree(newPos)
      } yield newPos -> (dist + 1)
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors -- visited.keys
      if (newToVisit.isEmpty || (toVisit.keySet intersect endPos).nonEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Map.empty, Map(startPos -> 0))
  }

  /** reachable units from a unit position
    */
  def getReachable(unit: CombatUnit, inRange: Set[Position])(implicit grid: Grid[Char], units: List[CombatUnit]): Map[Position, Int] = {
    bfs(unit.pos, inRange).filterKeys(inRange)
  }

  def getChosen(nearest: Set[Position]): Position = nearest.min

  def getNearest(reachable: Map[Position, Int]): Set[Position] = {
    val minDist = reachable.values.min
    reachable.filter(_._2 == minDist).keySet
  }

  /**
    * Calculate a step a unit has to take
    */
  def getStep(chosen: Position, unit: CombatUnit)(implicit grid: Grid[Char], units: List[CombatUnit]): Position = {
    val unitNeighbors = Position.axisOffsets.map(unit.pos + _).toSet
    val bfsMap = bfs(chosen, unitNeighbors)
    val neighborDists = bfsMap.filterKeys(unitNeighbors)
    val minDist = neighborDists.values.min
    neighborDists.filter(_._2 == minDist).keys.min
  }

  class ElfDeathException extends RuntimeException

  /**
    * Combate simulation
    * @param grid The grid the combat takes place in
    * @param units The list of units taking part
    * @param elfDeath whether or not the elves can lose the combat
    * @return Rounds the combat took and the number of units that remain.
    */
  def simulateCombat(grid: Grid[Char], units: List[CombatUnit], elfDeath: Boolean = false): (Int, List[CombatUnit]) = {

    def round(units: List[CombatUnit]): (List[CombatUnit], Boolean) = {

      def turn(init: List[CombatUnit], tail: List[CombatUnit], done: Boolean): (List[CombatUnit], Boolean) = tail match {
        case Nil => (init, done)
        case unit :: tl =>
          implicit val otherUnits: List[CombatUnit] = init ++ tl
          implicit val implicitGrid: Grid[Char] = grid
          val targets = getTargets(unit)
          val done2 = done || targets.isEmpty
          val inRange = getInRange(targets)

          var unit2 = unit
          if (!inRange.contains(unit.pos)) {
            val reachable = getReachable(unit, inRange)
            if (reachable.nonEmpty) {
              val nearest = getNearest(reachable)
              val chosen = getChosen(nearest)
              val step = getStep(chosen, unit)
              unit2 = unit.copy(pos = step)
            }
          }

          val unitNeighbors = Position.axisOffsets.map(unit2.pos + _).toSet
          val attackUnits = getTargets(unit2).filter(u => unitNeighbors.contains(u.pos))

          if (attackUnits.nonEmpty) {
            val attackUnit = attackUnits.minBy(u => (u.hp, u.pos))
            val attackUnit2 = attackUnit.copy(hp = attackUnit.hp - unit2.attackPower)
            val attackUnit3 = if (attackUnit2.hp > 0) Some(attackUnit2) else None
            if (elfDeath && attackUnit3.isEmpty && attackUnit2.unitType == Elf)
              throw new ElfDeathException

            val init2 = init.flatMap(u => if (u == attackUnit) attackUnit3 else Some(u))
            val tl2 = tl.flatMap(u => if (u == attackUnit) attackUnit3 else Some(u))
            turn(unit2 :: init2, tl2, done2)
          }
          else {
            turn(unit2 :: init, tl, done2)
          }
      }

      turn(Nil, units.sorted, done = false)
    }

    val roundIt = Iterator.iterate((units, false))({ case (units, _) => round(units) })
    val ((finalUnits, _), finalI) = roundIt.zipWithIndex.find({ case ((_, done), _) => done }).get

    (finalI - 1, finalUnits)
  }





  def combatOutcome(grid: Grid[Char], units: List[CombatUnit]): Int = {
    val (fullRounds, finalUnits) = simulateCombat(grid, units)
    val hpSum = finalUnits.map(_.hp).sum
    fullRounds * hpSum
  }
  def parseGrid(input: String): Grid[Char] = input.lines.map(_.toVector).toVector

  def parseUnit(cell: Char, pos: Position): Option[CombatUnit] = {
    val unitType: Option[UnitType] = cell match {
      case 'E' => Some(Elf)
      case 'G' => Some(Goblin)
      case _ => None
    }
    unitType.map(CombatUnit(_, pos))
  }
  def parseInput(input: String): (Grid[Char], List[CombatUnit]) = {
    val units: List[CombatUnit] = (for {
      (row, y) <- parseGrid(input).zipWithIndex
      (cell, x) <- row.zipWithIndex
      pos = Position(x, y)
      unit <- parseUnit(cell, pos)
    } yield unit).toList

    val grid = parseGrid(input.replace('E', '.').replace('G', '.'))

    (grid, units)
  }

  def combatOutcomeElfWin(grid: Grid[Char], units: List[CombatUnit]): Int = {
    def withElfAttackPower(elfAttackPower: Int): Option[Int] = {
      println("Elf attack power: " + elfAttackPower)

      val newUnits = units.map({
        case unit@CombatUnit(Elf, _, _, _) => unit.copy(attackPower = elfAttackPower)
        case unit => unit
      })

      Try(simulateCombat(grid, newUnits, elfDeath = true)).toOption.map({ case (fullRounds, finalUnits) =>
        val hpSum = finalUnits.map(_.hp).sum
        fullRounds * hpSum
      })
    }

    val attackPowers = (4 to 200).groupBy(e => math.ceil(200.0 / e).toInt).mapValues(_.min).values.toStream.sorted
    attackPowers.flatMap(withElfAttackPower).head
  }

  def combat(input: String): Int = {
    val (grid, units) = parseInput(input)
    combatOutcome(grid, units)
  }

  def combatElfWin(input: String): Int = {
    val (grid, units) = parseInput(input)
    combatOutcomeElfWin(grid, units)
  }

  def main(args: Array[String]): Unit = {
    lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("files/inputday15.txt")).mkString.stripLineEnd
    println(combat(input))
    println(combatElfWin(input))
  }
}
