import scala.io.Source

object Day7 {

  case class Task(from: Char, to: Char)

  val regex = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r

  case class Worker(char: Char, started: Int) {
    def requiredTime: Int = char - 'A' + 1 + 60

    def isFinished(time: Int): Boolean = time - started >= requiredTime
  }


  def taskSort(tasks: Seq[Task], set: Set[Char], order: String): String = {
    if (set.nonEmpty) {
      val nextTask = set.minBy(identity)
      val (newTasks, newSet) = tasks.filter(_.from == nextTask).foldLeft((tasks, set.filter(_ != nextTask))) {
        case ((tTasks, tSet), task) =>
          val filtered = tTasks.filter(_ != task)
          (filtered, if (filtered.forall(_.to != task.to)) tSet + task.to else tSet)
      }

      taskSort(newTasks, newSet, order + nextTask)
    } else
      order
  }

  def taskCompletionTime(tasks: Seq[Task], set: Set[Char], nWorkers: Int, taskOrder: String, time: Int, working: Seq[Worker]): Int = {
    if (set.nonEmpty || working.nonEmpty) {
      val (done, reduced) = working.partition(_.isFinished(time))
      val (tasksDone, setDone) = done.foldLeft((tasks, set)) { case ((cTasks, cSet), w) =>
        cTasks.filter(_.from == w.char).foldLeft((cTasks, cSet.filter(_ != w.char))) {
          case ((tTasks, tSet), task) =>
            val filtered = tTasks.filter(_ != task)
            (filtered, if (filtered.forall(_.to != task.to)) tSet + task.to else tSet)
        }
      }
      val (newSet, newTaskOrder, newWorking) = (working.size until nWorkers).foldLeft((setDone, taskOrder, reduced)) { case (id@(tSet, tOrder, tWorking), _) =>
        if (tSet.nonEmpty) {
          val n = tSet.minBy(identity)
          (tSet.filter(_ != n), tOrder + n, tWorking :+ Worker(n, time))
        } else
          id
      }
      taskCompletionTime(tasksDone, newSet, 5, newTaskOrder, time + 1, newWorking)
    }
    else
      time - 1 - 1
  }

  def main(args: Array[String]): Unit = {
    val tasks = Source.fromFile("src/main/scala/files/inputday7.txt").getLines.map {
      case regex(a, b) => Task(a.head, b.head)
    }.toSeq
    val initVertices = tasks.flatMap(e => Seq(e.from, e.to)).filter(c => tasks.forall(_.to != c)).toSet
    println(taskSort(tasks, initVertices, ""))
    val nworkers = 5
    println(taskCompletionTime(tasks, initVertices, nworkers, "", 0, Seq()))
  }
}
