package macphail

object Game extends Inputs with Rendering {

  trait GameStatus
  case object Running extends GameStatus
  case object Terminated extends GameStatus

  def run(world: World, commands: Vector[String]): (GameStatus, World) = {
    val (newWorld, inputMessages) = handleInputs(world, commands)

    val (newWorld2, gameStatus, updateMessages) = update(newWorld)

    render(newWorld2, inputMessages ++ updateMessages)

    (gameStatus, newWorld2)
  }

  def update(world: World): (World, GameStatus, Vector[String]) = {
    var messages = Vector[String]()

    val gameEnded = world.grid.forall(row => !row.contains(Symbols.blank))
    if(gameEnded) {
      messages :+= "Congratulations! You won!"
      (world, Terminated, messages)
    } else {
      messages :+= "Please enter a location like this: \"5 1,1\" (enter q to quit, ? for help)"
      (world, Running, messages)
    }
  }

}
