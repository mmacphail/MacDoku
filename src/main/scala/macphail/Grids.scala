package macphail

import scala.io.Source
import scala.util.Random

object Grids {

  val fileName = "grids.txt"

  def read(): Array[Array[String]] = {
    val lines = Source.fromInputStream(Grids.getClass.getClassLoader.getResourceAsStream(fileName)).getLines()

    val line = Random.shuffle(lines.toList).head

    (0 to 8).map(n => line.slice(n * 9, (n * 9) + 9).map(_.toString.replaceAll("0", Symbols.blank)).toArray).toArray
  }

}
