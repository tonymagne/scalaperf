import sbt._
import de.element34.sbteclipsify._

class ScalaperfProject(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
  val releases  = "releases" at "http://scala-tools.org/repo-releases"
  
  val specs2 = "org.specs2" % "specs2_2.9.0" % "1.3" % "test" withSources
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9" % "test" withSources
  val mockito = "org.mockito" % "mockito-all" % "1.8.5" % "test" withSources
  
  val commonsMath = "org.apache.commons" % "commons-math" % "2.2" withSources
  
  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)
}