import scala.collection.immutable.IndexedSeq
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
  private val rota = mutable.LinkedHashMap.empty[(Int, Int), mutable.Set[WorkerId]]
  private var shiftProperties = List.empty[collection.immutable.Set[String]]
  private var workerPreferences = collection.immutable.Map.empty[WorkerId, List[Int]]

  def allShifts: IndexedSeq[(Int, Int)] = (1 to nDays)
    .flatMap(day => (1 to shiftsPerDay).map(shift => (day, shift)))

  /**
    * Write an initial random solution
    */
  def init() {

    // calculations are 1-indexed
    allShifts
      .foreach{ case (day, shift) =>
        val currentShiftTeam = mutable.HashSet.empty[WorkerId]
        val availableWorkers = team.toBuffer
        for (i <- 1 to r.nextInt(math.min(maxGlobalTeamSize, team.size)) + 1) {
          val randomWorker: Int = r.nextInt(availableWorkers.size)
          currentShiftTeam.add(availableWorkers.remove(randomWorker))
        }
          rota += (day, shift) -> currentShiftTeam
    }
  }

  def get(): collection.immutable.Map[(Int, Int), collection.immutable.Set[WorkerId]] = {
    allShifts
      .map{ case (day, shift) =>
        (day, shift) -> rota(day, shift).toSet
      }.toMap[(Int, Int), collection.immutable.Set[WorkerId]]
  }

  def randomShift = (r.nextInt(nDays) + 1, r.nextInt(shiftsPerDay) + 1)

  def drawRandomWorkerFromRandomShift(): Option[WorkerId] = {
    val randomShift1: (Int, Int) = randomShift
    var shift = rota(randomShift1)
    var trial = 1; val maxTrials = 5
    while (shift.isEmpty || trial > maxTrials) {
      trial += 1
      shift = rota(randomShift)
    }
    if (shift.isEmpty) {
      Option.empty[WorkerId]
    } else {
      val teamSize = shift.size
      val pickedWorker = shift.iterator.drop(r.nextInt(teamSize)).next
      shift.remove(pickedWorker)
      Option(pickedWorker)
    }
  }

  def addWorkerToRandomShift(worker: WorkerId) {
    val shift = rota(randomShift)
    shift += worker
  }

  def swap() {
    val maybeWorker = drawRandomWorkerFromRandomShift()

    if (maybeWorker.isDefined) {
      val worker = maybeWorker.get
      var shift = rota(randomShift)
      // if the worker is already present take another shift
      while (shift.contains(worker)) {
        shift = rota(randomShift)
      }
      shift += worker
    }

  }

  def addRandomWorkerToRandomShift() {
    val randomWorker: WorkerId = team.iterator.drop(r.nextInt(team.size)).next
    addWorkerToRandomShift(randomWorker)
  }

  def removeRandomWorkerFromRandomShift() {
    drawRandomWorkerFromRandomShift()
  }

  def pickNeighborhood(): () => () => Unit = {
    val neighborhoods = Set(removeRandomWorkerFromRandomShift _, addRandomWorkerToRandomShift _, swap _)
    neighborhoods.iterator.drop(r.nextInt(neighborhoods.size)).next
  }

  def setPreferencesFor(workerId: WorkerId, preferences: Map[(Int, Int), Int]) {
    val preferenceList = allShifts
      .map{ case (day, shift) =>
        preferences((day, shift))
    }.toList
    workerPreferences += (workerId -> preferenceList)
  }

  def setShiftProperties(properties: Map[(Int, Int), Set[String]]) {
    shiftProperties = properties.values.toList
  }

  def getPresenceListFor(workerId: WorkerId): List[Int] = {
    allShifts
      .map{ case (day, shift) =>
          if (rota((day, shift)) contains workerId) 1 else 0
      }.toList
  }

  def loss(): Int = {
    val point = get

  }
}

case class WorkerId(id: Int)