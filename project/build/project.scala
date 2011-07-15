import sbt._

final class MolleProject(info: ProjectInfo) extends AppengineProject(info) { 
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  
  val guava = "com.google.guava" % "guava" % "r09"
  val counterfeiter = "org.bifrost" %% "counterfeiter" % "1.0"
  val scalatestDependency = "org.scalatest" %% "scalatest" % "1.6.1" % "test"
  val apachePoi = "org.apache.poi" % "poi" % "3.7"
  val apachePoiScratchpad = "org.apache.poi" % "poi-scratchpad" % "3.7"
  val apacheCommonsIo = "commons-io" % "commons-io" % "2.0.1"
  val httpClient = "org.apache.httpcomponents" % "httpclient" % "4.1.1"
  val httpMime = "org.apache.httpcomponents" % "httpmime" % "4.1.1"

  override def testClasspath = super.testClasspath +++ (path("lib_test") * "*.jar")
}

