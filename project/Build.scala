import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"
  val appVersion      = "0-SNAPSHOT"

  val dbDependencies = Seq(
    jdbc,
    anorm,
    "org.postgresql" % "postgresql" % "9.2-1003-jdbc4"
  )

  val avDependencies = Seq(
    "commons-io" % "commons-io" % "2.4"
  )

  val dbrary = Project("dbrary", file("dbrary")).settings(
    libraryDependencies ++= dbDependencies
  )

  val media = Project("media", file("media")).dependsOn(dbrary).settings(
    libraryDependencies ++= avDependencies
  )

  val appDependencies = dbDependencies ++ avDependencies ++ Seq(
    "org.mindrot" % "jbcrypt" % "0.3m"
  )

  val main = play.Project(appName, appVersion, appDependencies).
    dependsOn(dbrary, media)

}
