import macphail.{GridOps, World}
import org.scalatest._

class GridOpsTest extends FlatSpec with Matchers with EitherValues with GridTestSupport with GridOps {

  "A number" should "be inserted in the grid" in {
    val expectedGrid = create((3, 0, 0))
    val result = insertAt(World(), 3, (0, 0))
    result.right.value.grid should equalsGrid(expectedGrid)

    update(expectedGrid, (2, 1, 3))
    val result2 = insertAt(result.right.value, 2, (1, 3))
    result2.right.value.grid should equalsGrid(expectedGrid)
  }
}
