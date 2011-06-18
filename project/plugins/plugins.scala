import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val appengine_plugin = "net.stbbs.yasushi" % "sbt-appengine-plugin" % "2.2.1"
}

