package macphail

import scala.util.Try
import cats.implicits._
import cats.data.Writer

trait Inputs extends GridOps {

  type InputOp[A] = Either[String, A]
  type Logger[A] = Writer[Vector[String], A]

  def handleInputs(world: World, commands: Vector[String]): (World, Vector[String]) = {
    val logger = for {
      commands <- parseCommands(commands)
      world    <- runCommands(world, commands)
    } yield world
    val logs = logger.written
    val finalWorld = if(logs.isEmpty) logger.value else logger.value.copy(refresh = true)
    (finalWorld, logs)
  }

  def parseCommands(stringCommands: Vector[String]): Logger[Vector[Command]] = {
    stringCommands.foldLeft(Vector[Command]().pure[Logger])
      {(logger, str) => {
        opToLogger(parseCommand(str), logger, (cmd: Command, cmds) => cmds :+ cmd)
    }}
  }

  def opToLogger[A,B](result: InputOp[A], logger: Logger[B], fn: (A,B) => B) = {
    result.fold(error => logger.tell(Vector(error)), a => logger.map(fn(a, _)))
  }

  def runCommands(world: World, commands: Vector[Command]): Logger[World] = {
    commands.foldLeft(world.pure[Logger])
      {(logger, command) => {
        val currentWorld: World = logger.value
        command match {
          case PutNumber(number, position) =>
            opToLogger(insertAt(currentWorld, number, position), logger, (w: World, _) => w.copy(refresh = true))
          case EraseNumber(position)       =>
            opToLogger(eraseAt(currentWorld, position), logger, (w: World, _) => w.copy(refresh = true))
          case Help                        =>
            logger.map(_.copy(refresh = true)).tell(printHelp)
        }
    }}
  }

  def notAValidInput(str: String) = Left(s"$str is not a valid input... try again !")

  def parseCommand(str: String): InputOp[Command] = {
    if(str.startsWith("e")) {
      parseErase(str)
    } else if(str.startsWith("?")) {
      Right(Help)
    } else {
      parsePutNumber(str)
    }
  }

  def parsePutNumber(str: String): InputOp[Command] = {
    Try[Command] {
      val parts = str.split(" ")
      val number = parts(0).toInt
      val pos = parts(1).split(",") match {
        case Array(x,y) => (x.toInt - 1, y.toInt - 1)
      }
      PutNumber(number, pos)
    }.fold(_ => notAValidInput(str), Right(_))
  }

  def parseErase(str: String): InputOp[Command] = {
    Try[Command] {
      val pos = str.drop(2).split(",") match {
        case Array(x,y) => (x.toInt - 1, y.toInt - 1)
      }
      EraseNumber(pos)
    }.fold(_ => notAValidInput(str), Right(_))
  }

  lazy val printHelp: Vector[String] = {
    """|Available commands :
      |?     - print this help
      |q     - quit
      |e x,y - erase a number at position (x,y)
      |n x,y - put a number n at position (x,y)
    """.stripMargin.split("\\\n").toVector
  }
}
