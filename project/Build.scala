import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"
  val appVersion      = "0-SNAPSHOT"

  val dbDependencies = Seq(
    jdbc,
    anorm,
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4" // "9.2-1002.jdbc4"
  )

  val dbrary = Project("dbrary", file("dbrary")).settings(
    libraryDependencies ++= dbDependencies
  )

  val appDependencies = dbDependencies ++ Seq(
  )

  val main = play.Project(appName, appVersion, appDependencies).
    dependsOn(dbrary)

}
