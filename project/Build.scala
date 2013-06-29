import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"
  val appVersion      = "0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm
  )

  val dbrary = Project("dbrary", file("dbrary"))

  val main = play.Project(appName, appVersion, appDependencies).dependsOn(dbrary).settings(
    templatesImport += "dbrary.Permission"
  )

}
