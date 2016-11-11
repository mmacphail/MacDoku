package macphail

case class World(
                  grid: Array[Array[String]] = Array.fill(9,9)(Symbols.blank),
                  startingGrid: Array[Array[String]] = Array.fill(9,9)(Symbols.blank),
                  refresh: Boolean = true
                )
