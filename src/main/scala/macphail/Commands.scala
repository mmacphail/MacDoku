package macphail

sealed trait Command
case class PutNumber(number: Int, position: (Int, Int)) extends Command
case class EraseNumber(position: (Int, Int)) extends Command
case object Help extends Command