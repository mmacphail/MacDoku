package macphail

trait GridCopy {
  def copy(grid: Array[Array[String]]): Array[Array[String]] = {
    val gridCopy = Array.fill[String](9,9)(Symbols.blank)
    (0 to 8).foreach(col => {
      (0 to 8).foreach(row => {
        gridCopy(col)(row) = grid(col)(row)
      })
    })
    gridCopy
  }
}
