package waterplanner

import org.optaplanner.core.api.domain.entity.PlanningEntity
import org.optaplanner.core.api.domain.solution.{PlanningEntityCollectionProperty, PlanningScore, PlanningSolution}
import org.optaplanner.core.api.domain.solution.drools.ProblemFactCollectionProperty
import org.optaplanner.core.api.domain.valuerange.ValueRangeProvider
import org.optaplanner.core.api.domain.variable.PlanningVariable
import org.optaplanner.core.api.score.Score
import org.optaplanner.core.api.score.buildin.hardsoft.HardSoftScore
import org.optaplanner.core.impl.score.director.easy.EasyScoreCalculator

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.collection.mutable

class WaterSolutionScore extends EasyScoreCalculator[WaterProblem] {
  def forceRV(waterProblem: WaterProblem): HardSoftScore = {
    var hardScore = 0
    for (grain <- waterProblem.usageGrains.asScala) {
      if (grain.day < 3) {
        if (grain.source != null      && !grain.source.name.contains("rv"))      hardScore += 1
        if (grain.destination != null && !grain.destination.name.contains("rv")) hardScore += 1
      }
    }
    HardSoftScore.valueOf(-hardScore, 0)
  }

  def checkGrainExists(waterProblem: WaterProblem): HardSoftScore = {
    var hardScore = 0
    for (grain <- waterProblem.usageGrains.asScala) {
      if (grain.source == null) {
        hardScore += 1
      } else {
        if (grain.source.isGrey) hardScore += 1
      }

      if (grain.destination == null) {
        hardScore += 1
      } else {
        if (!grain.destination.isGrey) hardScore += 1
      }
    }
    HardSoftScore.valueOf(-hardScore, 0)
  }

  def verifyCapacity(waterProblem: WaterProblem): HardSoftScore = {
    var hardScore = 0
    val sources = waterProblem.usageGrains.collect{
      case g if g.source != null => g.source -> g
    }.groupBy(_._1).mapValues(_.unzip._2.toList)
    val dests = waterProblem.usageGrains.collect{
      case g if g.destination != null => g.destination -> g
    }.groupBy(_._1).mapValues(_.unzip._2.toList)

    for ((source, uses) <- sources) {
      val totalSize = uses.map(_.waterUse).sum
      if (totalSize > source.capacity) hardScore += 1
    }
    for ((source, uses) <- dests) {
      val totalSize = uses.map(_.greyWater).sum
      if (totalSize > source.capacity) hardScore += 1
    }

    HardSoftScore.valueOf(-hardScore, 0)
  }

  def neatness(waterProblem: WaterProblem): HardSoftScore = {
    var softScore = 0
    for ((a, b) <- waterProblem.usageGrains.zip(waterProblem.usageGrains.tail)) {
      if (a.source != b.source) softScore += 1
      if (a.destination != b.destination) softScore += 1
    }
    HardSoftScore.valueOf(0, -softScore)
  }

  def ensureNoverlap(waterProblem: WaterProblem): HardSoftScore = {
    try {
      val lastFresh = waterProblem.usageGrains.zipWithIndex.filter { case (use, ix) =>
        use.source != null && use.source.name == "barrel 1  "
      }.map(_._2).max
      val firstGrey = waterProblem.usageGrains.zipWithIndex.filter { case (use, ix) =>
        use.destination != null && use.destination.name == "grey barre"
      }.map(_._2).min
      val score = if (lastFresh >= firstGrey) 1 else 0
      HardSoftScore.valueOf(-score, 0)
    } catch {
      case e: java.lang.UnsupportedOperationException => HardSoftScore.ZERO
    }
  }

  def calculateScore(problem: WaterProblem): Score[_] = {
    var hardScore = 0
    var softScore = 0

    val containerUsage = mutable.HashMap[String, Double](problem.containers.map(v => v.name -> v.capacity.toDouble): _*)

    for (grain <- problem.usageGrains) {
      if (grain.source == null) {
        hardScore += 1
      } else {
        if (grain.source.isGrey) hardScore += 1
        if (grain.day < 3 && !(
            grain.source.name.contains("rv") ||
            grain.source.name.contains("boxen"))) {
          hardScore += 1
        }
        containerUsage.update(grain.source.name, containerUsage(grain.source.name) - grain.waterUse)
      }

      if (grain.destination == null) {
        hardScore += 1
      } else {
        if (!grain.destination.isGrey) hardScore += 1
        if (grain.day < 3 && !grain.destination.name.contains("rv")) {
          hardScore += 1
        }
        containerUsage.update(grain.destination.name, containerUsage(grain.destination.name) - grain.greyWater)
      }

      containerUsage.update(
        "rv fresh  ",
        containerUsage("rv fresh  ") - 3*grain.showers
      )
      containerUsage.update(
        "rv - grey ",
        containerUsage("rv - grey ") - 3*grain.showers
      )
    }

    for ((source, sourceV) <- containerUsage) {
      if (sourceV < 0){
        if (source.contains("grey") || source.contains("black")) softScore += 100
        else hardScore += 1
      }
    }

    for ((g1, g2) <- problem.usageGrains.zip(problem.usageGrains.tail)) {
      if (g1.source != g2.source) softScore += 1
      if (g1.destination != g2.destination) softScore += 1
    }

    HardSoftScore.valueOf(-hardScore, -softScore).add(ensureNoverlap(problem))
  }
}

class WaterContainer(val name: String,
                     val capacity: Integer,
                     var isGrey: Boolean) {
  def this() = this(null, 0, true)
}

@PlanningEntity
class WaterUseDay(val day: Int, val waterUse: Double, val greyWater: Double, val showers: Int,
                  _source: WaterContainer, _dest: WaterContainer) {
  @PlanningVariable(valueRangeProviderRefs = Array("containers")) var destination: WaterContainer = _dest
  @PlanningVariable(valueRangeProviderRefs = Array("containers")) var source: WaterContainer = _source
  def this() = this(0, 0, 0, 0, null, null)
}

object WaterUseDay {
  def fromPeople(day: Int, n: Int, showers: Int) = new WaterUseDay(day, n*1.5, n*0.75, showers, null, null)
}

@PlanningSolution
class WaterProblem(private val _containers: List[WaterContainer], private val _grains: List[WaterUseDay]) {
  def this() = this(null, null)

  @ValueRangeProvider(id = "containers") @ProblemFactCollectionProperty
  val containers: java.util.List[WaterContainer] = _containers.asJava
  @PlanningEntityCollectionProperty
  val usageGrains: java.util.List[WaterUseDay] = _grains.asJava
  @PlanningScore var score: HardSoftScore = _

  override def toString: String = {
    val cs = containers
    val gs = usageGrains
    val contS = cs.map {c =>
      var total = 0d
      val holdings = gs.map{g =>
        (g.source, g.destination) match {
          case (a, b) if a == c && b == c =>
            total += g.greyWater
            total += g.waterUse
            "x"
          case (a, b) if a == c =>
            total += g.waterUse
            "s"
          case (a, b) if b == c =>
            total += g.greyWater
            "d"
          case _ => " "
        }
      }
      s"${c.name}: " + holdings.mkString("") + s"\t$total/${c.capacity}"
    }
    val s = contS.mkString("\n") + "\n"
    s
  }
}