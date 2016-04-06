import scala.collection.mutable

/**
  * Created by gurghet on 05.04.16.
  */
class Rota(nDays: Int, team: Set[WorkerId]) {
  private val shiftsPerDay = 3
  private val maxGlobalTeamSize = 5
  private val r = scala.util.Random

  // all the shifts have an implicit day and order
  // for example the with 3 shifts per day the 4th
  // shift is the 1st shift of the second day
  private val shifts = mutable.LinkedHashMap.empty[(Int, Int), Set[WorkerId]]

  /**
    * Write an initial random solution
    */
  def init() {

    // calculations are 1-indexed
    (1 to nDays)
      .flatMap(day => (1 to shiftsPerDay).map(shift => (day, shift)))
      .foreach{ case (day, shift) =>
        val currentShiftTeam = mutable.Set.empty[WorkerId]
        val availableWorkers = team.toBuffer
        for (i <- 1 to r.nextInt(math.min(maxGlobalTeamSize, team.size))) {
          val randomWorker: Int = r.nextInt(availableWorkers.size)
          currentShiftTeam.add(availableWorkers.remove(randomWorker))
        }
          shifts += (day, shift) -> currentShiftTeam.toSet
    }
  }

  def get(): collection.Map[(Int, Int), collection.Set[WorkerId]] = {
    (1 to nDays)
      .flatMap(day => (1 to shiftsPerDay).map(shift => (day, shift)))
      .map{ case (day, shift) =>
        (day, shift) -> shifts(day, shift)
      }.toMap[(Int, Int), collection.Set[WorkerId]]
    // todo: convert to immutable
  }

  def randomShift = (r.nextInt(nDays) + 1, r.nextInt(shiftsPerDay) + 1)

  def drawRandomWorker()(implicit : WorkerId = {
    val shift = shifts(randomShift)
    val teamSize = shift.size
    shift.iterator.drop(r.nextInt(teamSize)).next
  }

  def addWorkerToShift(shift: (Int, Int)) {
    val shift = shifts(randomShift)
  }

  def swap(): Unit = {


  }
}

case class WorkerId(id: Int)