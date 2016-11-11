name := "MacDoku"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies := Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.12",
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

mainClass in Compile := Some("macphail.Sudoku")