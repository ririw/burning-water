package waterplanner

import java.net.URI
import java.nio.file
import scala.collection.JavaConverters._

import org.optaplanner.core.api.solver.{Solver, SolverFactory}

/**
  * Created by richardweiss on 5/8/17.
  */
object Main extends App {
  def makeProblem(): WaterProblem = {
    val containers = List(
      new WaterContainer("barrel 1  ", 55, false),
      new WaterContainer("barrel 2  ", 55, false),
      new WaterContainer("rv fresh  ", 55, false),
      new WaterContainer("boxen     ", 36, false),
      new WaterContainer("rv - black", 21, true),
      new WaterContainer("rv - grey ", 28*2, true),
      new WaterContainer("grey barre", 55, true)
    )
    val useGrains = List(
      WaterUseDay.fromPeople(1,  5,  0),
      WaterUseDay.fromPeople(2,  5,  0),
      WaterUseDay.fromPeople(3,  12, 1),
      WaterUseDay.fromPeople(4,  12, 0),
      WaterUseDay.fromPeople(5,  12, 1),
      WaterUseDay.fromPeople(6,  12, 1),
      WaterUseDay.fromPeople(7,  12, 1),
      WaterUseDay.fromPeople(8,  12, 0),
      WaterUseDay.fromPeople(9,  12, 1),
      WaterUseDay.fromPeople(10, 12, 0),
      WaterUseDay.fromPeople(11, 12, 1)
    )
    new WaterProblem(containers, useGrains)
  }

  val problem = makeProblem()
  // Work around SBT's derpyness with resource and class loading.
  // https://stackoverflow.com/questions/6485880/how-can-i-access-a-resource-when-running-an-sbt-runtask
  val configStream = Class.forName("waterplanner.Main").getResourceAsStream("/waterplanner/solverconfig.xml")
  assert(configStream != null)
  val solverFactory: SolverFactory[WaterProblem] = SolverFactory.createFromXmlInputStream(configStream)
  val solver: Solver[WaterProblem] = solverFactory.buildSolver
  val solvedWaterProblem = solver.solve(problem)
  val ws = new WaterSolutionScore()
  val score = ws.calculateScore(solvedWaterProblem)

  println("Graincheck: ", ws.checkGrainExists(solvedWaterProblem))
  println("Capacity:   ", ws.verifyCapacity(solvedWaterProblem))
  println("neatness:   ", ws.neatness(solvedWaterProblem))
  println("Noverlap:   ", ws.ensureNoverlap(solvedWaterProblem))
  println("Total:      ", score.toShortString)
  print(solvedWaterProblem)
}
