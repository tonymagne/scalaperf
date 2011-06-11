name := "scalaperf"

version := "0.1.0"

organization := "org.scalaperf"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math" % "2.2",
  "org.specs2" %% "specs2" % "1.4" % "test",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2" % "test"
)

parallelExecution in Test := false

initialCommands in console := "import org.scalaperf"

