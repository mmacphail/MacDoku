package macphail

import akka.actor.{ActorSystem, Props}

import scala.io.Source

object Sudoku extends App {

  val system = ActorSystem("yutani")
  val game = system.actorOf(Props[GameActor])

  Source.stdin.getLines().takeWhile(_ != "q").foreach { ln =>
    game ! GameActor.Input(ln)
  }

  system.terminate()
}
