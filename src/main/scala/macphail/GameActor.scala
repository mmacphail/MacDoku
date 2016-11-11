package macphail

import akka.actor.Actor

import scala.concurrent.duration._

object GameActor {
  object HasGameEnded

  trait GameResult
  object GameEnded extends GameResult
  object GameHasNotEnded extends GameResult

  case class Input(line: String)
}

class GameActor extends Actor with GridCopy {
  import GameActor._

  implicit val ec = context.dispatcher
  val scheduledTask = context.system.scheduler.schedule(1.seconds, 10.milliseconds, self, Tick)
  var world = init()
  var commands = Vector[String]()
  var lastGrid = Vector[String]()
  var lastMessages = Vector[String]()
  var gameEnded = false

  object Tick

  def receive = {
    case Tick =>
      if(!gameEnded) {
        val (status, newWorld) = Game.run(world, commands)
        world = newWorld.copy(refresh = false)
        if(status == Game.Terminated) {
          gameEnded = true
          self ! GameEnded
        }

        flushCommands()
      }

    case GameEnded =>
      scheduledTask.cancel()

    case HasGameEnded =>
      if(gameEnded)
        sender() ! GameEnded
      else
        sender() ! GameHasNotEnded

    case Input(line) =>
      commands :+= line
  }

  def init(): World = {
    val grid = Grids.read()
    val startingGrid = copy(grid)
    World(grid, startingGrid)
  }

  def flushCommands() = {
    commands = Vector()
  }
}
