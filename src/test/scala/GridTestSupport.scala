import macphail.{Rendering, Symbols, World}
import org.scalatest.matchers.{MatchResult, Matcher}

trait GridTestSupport extends Rendering {

  def create(args: (Int, Int, Int)*): Array[Array[String]] = {
    val g = Array.fill[String](9,9)(Symbols.blank)
    args.foreach {
      case (n, x, y) =>
        g(y)(x) = n.toString
    }
    g
  }

  def update(grid: Array[Array[String]], args: (Int, Int, Int)*): Array[Array[String]] = {
    args.foreach {
      case (n, x, y) =>
        grid(y)(x) = n.toString
    }
    grid
  }

  def printWorld(world: World): Unit = render(world)

  def printGrid(grid: Array[Array[String]]): Unit = render(World(grid))

  class EqualsGrid(grid: Array[Array[String]]) extends Matcher[Array[Array[String]]] {
    def apply(left: Array[Array[String]]): MatchResult = {
      MatchResult(
        grid.deep == left.deep,
        "The grids are not equal",
        "The grids are equal"
      )
    }
  }

  def equalsGrid(expectedGrid: Array[Array[String]]) = new EqualsGrid(expectedGrid)
}
