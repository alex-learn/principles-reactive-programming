package simulations

import math.random

class EpidemySimulator extends Simulator with PersonLogic {

  def randomBelow(i: Int) = (random * i).toInt
  def randomBoolean: Boolean = randomBelow(2) == 1
  def oneIn(i: Double) = randomBelow((1 / i).toInt) == 0

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val survivalRate = 0.25
    def incubationPeriod = 6
    def deathRiskDay = 14
    def immuneDay = 16
    def healDay = 18
    val dayDelay = 1
    val maxRandomDayOfMoving = 5

    //	Extensions
    val airTrafficEnabled = false
    val airTrafficRate = 0.01

    val reduceMobilityActEnabled = false
    val mobilityReductionRate = 0.5

    val chosenFewActEnabled = false
    val chosenFewRate = 0.05
  }

  import SimConfig._

  val persons: List[Person] = (for {
    i <- (1 to population)
  } yield {
    val person = new Person(i)
    person.infected = (i % ((1 / prevalenceRate).toInt)) == 0
    person.vaccinated = (i % ((1 / chosenFewRate).toInt)) == 0
    person
  }).toList

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    var daysWithoutMoving: Int = 0

    var daysSinceInfection: Int = 0

    var vaccinated = false

    def waitUntilTomorrow {
      afterDelay(dayDelay) { personAction(this) }
    }

  }

  import scala.util.Random.shuffle

  def movement(implicit person: Person) {
    if (person.needsToMove && !person.dead) {
      val myValidNeighbours = validNeighbours(person.position, neighbours(person.position))
      if (airTrafficEnabled && oneIn(airTrafficRate))
        person moveTo ((randomBelow(roomRows), randomBelow(roomColumns)))
      else if (canMove attendingTo myValidNeighbours)
        person moveTo myValidNeighbours(randomBelow(myValidNeighbours.size))
    } else
      person.daysWithoutMoving -= 1
  }

  def updateStatus(implicit person: Person) {
    (person.infected, person.sick, person.immune, person.dead) match {
      case (true, false, false, false) =>
        // Person becomes sick if he was previously infected
        if (person.daysSinceInfection == incubationPeriod) person.sick = true
        person.daysSinceInfection += 1
      case (true, true, false, false) =>
        if (person.mayDie && person.hasToDie)
          // Person dies
          person.dead = true
        if (person.daysSinceInfection == immuneDay)
          // Person becomes immune
          person.immune = true
        person.daysSinceInfection += 1
      case (true, true, true, false) =>
        // Person gets healthy
        if (person.daysSinceInfection < healDay) person.daysSinceInfection += 1
        else {
          person.daysSinceInfection = 0
          person.infected = false
          person.sick = false
          person.immune = false
        }
      case _ => ()
    }
  }

  def personAction(person: Person) {
    implicit val _ = person
    //Do person stuff ...
    movement
    updateStatus
    //... and tomorrow never ends.
    person waitUntilTomorrow
  }

  persons.foreach(_.waitUntilTomorrow)

}
trait PersonLogic {
  self: EpidemySimulator =>

  import SimConfig._

  type GridPosition = (Int, Int)

  def normalize(i: Int, maxRank: Int): Int = if (i == maxRank) 0 else if (i < 0) maxRank - 1 else i

  implicit class IsInfectedHelper(pos: GridPosition) {
    def isInfected: Boolean = pos match {
      case (row, col) => persons.exists(p => p.row == row && p.col == col && (p.sick || p.dead))
    }
  }

  implicit class PersonHelper(person: Person) {
    def position: GridPosition = (person.row, person.col)
    def needsToMove: Boolean = person.daysWithoutMoving < 1
    def mayBeInfected: Boolean =
      !person.infected && persons.exists(p => p.row == person.row && p.col == person.col && (p.infected || p.dead) && !person.vaccinated)
    def mayDie: Boolean = person.daysSinceInfection == deathRiskDay
    def hasToDie: Boolean = oneIn(0.5)
    def moveTo(pos: GridPosition) {
      pos match {
        case (newRow, newCol) =>
          person.row = newRow
          person.col = newCol
          person.daysWithoutMoving = randomBelow(maxRandomDayOfMoving) + 1
          if (person mayBeInfected) person.infected = oneIn(transmissibilityRate)
      }
    }
  }

  def neighbours(position: GridPosition): List[GridPosition] = position match {
    case (actualRow, actualCol) =>
      (for {
        row <- List(actualRow - 1, actualRow, actualRow + 1)
        col <- List(actualCol - 1, actualCol, actualCol + 1)
      } yield (row, col))
        .filter { case pos @ (row, col) => pos != position && (row != actualRow ^ col != actualRow) }
        .map { case (row, col) => (normalize(row, roomRows), normalize(col, roomColumns)) }
  }

  def validNeighbours(position: GridPosition, neighbours: List[GridPosition]): List[GridPosition] = position match {
    case (actualRow, actualCol) =>
      neighbours.filterNot(_ isInfected) match {
        case validNeighbours =>
          if (!reduceMobilityActEnabled) validNeighbours
          else scala.util.Random.shuffle(validNeighbours).take(validNeighbours.size / (1 / mobilityReductionRate).toInt)
      }
  }

  object canMove {
    def attendingTo(validNeighbours: List[GridPosition]): Boolean = !validNeighbours.isEmpty
  }

}
