name := "scalaperf"

version := "0.1.1"

organization := "org.scalaperf"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math" % "2.2",
  "org.specs2" %% "specs2" % "1.6" % "test",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"
)

parallelExecution in Test := false

initialCommands in console := "import org.scalaperf"

publishTo := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/")

credentials += Credentials(Path.userHome / ".ivy2" / "scala-tools.credentials")