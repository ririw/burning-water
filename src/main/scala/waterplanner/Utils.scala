package waterplanner
import scala.collection.JavaConverters._
/**
  * Created by richardweiss on 6/8/17.
  */
object Utils {
  def problemToString(waterProblem: WaterProblem): String = {
    val cs = waterProblem.containers.asScala
    val gs = waterProblem.usageGrains.asScala
    val showerString = "        showers: |" + gs.map(_.showers).map{
      case 0 => " "
      case 1 => "▄"
      case 2 => "█"
      case v => v.toString
    }.mkString("") + "|"
    val contS = cs.map {c =>
      var total = 0d
      val holdings = gs.map{g =>
        (g.source, g.dest) match {
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

      val showerHoldings: Int = if (c.isInstanceOf[RVWater] || c.isInstanceOf[RVGreyWater]) {
        gs.map{g => g.showers * 3 }.sum
      } else {
        0
      }
      total += showerHoldings
      assert(c.name.length < 15) // prevent messy formatting, and _surely_ 15 is enough...
      val name = "%15s".format(c.name)
      s"$name: |" + holdings.mkString("") + s"|\t$total/${c.capacity}"
    }
    val s = showerString + "\n" + contS.mkString("\n") + "\n"
    s
  }
}
