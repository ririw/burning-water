package waterplanner

import org.optaplanner.core.api.solver.{Solver, SolverFactory}

/**
  * Created by richardweiss on 5/8/17.
  */
object Main extends App {
  def makeProblem(): WaterProblem = {
    val containers = List(
      new WaterBarrel(1),
      new WaterBarrel(2),
      new Boxes(12 * 2 + 5 * 3),
      new GreywaterBarrel(1),
      new GreywaterBarrel(2),
      new RVBlackWater(),
      new RVGreyWater(2),
      new RVWater()
    )
    val useGrains = List(
      WaterUseDay.fromPeople(1,  5),
      WaterUseDay.fromPeople(2,  5),
      WaterUseDay.fromPeople(3,  12),
      WaterUseDay.fromPeople(4,  12),
      WaterUseDay.fromPeople(5,  12),
      WaterUseDay.fromPeople(6,  12),
      WaterUseDay.fromPeople(7,  12),
      WaterUseDay.fromPeople(8,  12),
      WaterUseDay.fromPeople(9,  12),
      WaterUseDay.fromPeople(10, 12),
      WaterUseDay.fromPeople(11, 12)
    )
    new WaterProblem(5, containers, useGrains)
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
  // Before/after
  println(ws.scoreString(problem))
  print(problem)
  println("----------")
  println(ws.scoreString(solvedWaterProblem))
  print(solvedWaterProblem)
}
