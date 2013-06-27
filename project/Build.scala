import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"
  val appVersion      = "0-SNAPSHOT"

  val appDependencies = Seq(
    "com.typesafe.play" %% "play-slick" % "0.3.2",
    "com.typesafe.slick" %% "slick" % "1.0.1",
    "fi.reaktor" %% "sqltyped" % "0.3.0-SNAPSHOT"
  )

  val dbrary = Project("dbrary", file("dbrary"))

  val main = play.Project(appName, appVersion, appDependencies).dependsOn(dbrary)

}
