package macphail

trait Rendering {

  def render(world: World, messages: Vector[String] = Vector()): Unit = {
    if(world.refresh) {
      val gridGraphics = world.grid.map(_.mkString(Symbols.border, Symbols.border, Symbols.border)).toVector
      println(gridGraphics.mkString("\n"))
      println("\n" + messages.mkString("\n"))
    }
  }

}
