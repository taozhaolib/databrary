import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"

  val dbDependencies = Seq(
    jdbc,
    "com.github.mauricio" %% "postgresql-async" % "0.2.10"
  )

  val avDependencies = Seq(
    "commons-io" % "commons-io" % "2.4"
  )

  val macros = Project("macros", file("macros")).settings(
    libraryDependencies +=
      "org.scala-lang" % "scala-reflect" % "2.10.3"
  )

  val dbrary = Project("dbrary", file("dbrary")).dependsOn(macros).settings(
    libraryDependencies ++= dbDependencies ++ Seq(
      component("play"),
      "org.postgresql" % "postgresql" % "9.2-1003-jdbc4"
    )
  )

  val media = Project("media", file("media")).dependsOn(dbrary).settings(
    libraryDependencies ++= avDependencies
  )

  val appDependencies = dbDependencies ++ avDependencies ++ Seq(
    "org.mindrot" % "jbcrypt" % "0.3m",
    "com.typesafe" %% "play-plugins-mailer" % "2.2.0"
  )

  val main = play.Project(appName, "unknown", appDependencies).
    dependsOn(macros, dbrary, media).settings(
      version <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown"))
    )
}
