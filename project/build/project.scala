import sbt._

final class MolleProject(info: ProjectInfo) extends AppengineProject(info) { 
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  
  val guava = "com.google.guava" % "guava" % "r09"
  val counterfeiter = "org.bifrost" %% "counterfeiter" % "1.0"
  val scalatestDependency = "org.scalatest" % "scalatest" % "1.2" % "test"
  val junit = "junit" % "junit" % "4.8.1" % "test"
}

