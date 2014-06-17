import sbt._
import play.PlayImport._
import Keys._

object ApplicationBuild extends Build {

  val appName         = "databrary"

  val dbDependencies = Seq(
    jdbc,
    "com.github.mauricio" %% "postgresql-async" % "0.2.14-SNAPSHOT"
  )

  val avDependencies = Seq(
    "commons-io" % "commons-io" % "2.4"
  )

  val macros = Project("macros", file("macros")).settings(
    libraryDependencies +=
      "org.scala-lang" % "scala-reflect" % "2.11.1"
  )

  val dbrary = Project("dbrary", file("dbrary")).dependsOn(macros).settings(
    libraryDependencies ++= dbDependencies ++ Seq(
      component("play"),
      "org.postgresql" % "postgresql" % "9.3-1101-jdbc41"
    )
  )

  val media = Project("media", file("media")).dependsOn(dbrary).settings(
    libraryDependencies ++= avDependencies
  )

  val logbackAccess = Project("logback-access", file("logback-access"))

  val appDependencies = dbDependencies ++ avDependencies ++ Seq(
    "org.mindrot" % "jbcrypt" % "0.3m",
    ws,
    "com.typesafe.play.plugins" %% "play-plugins-mailer" % "2.3.0"
  )

  val main = Project(appName, file("."))
    .enablePlugins(play.PlayScala)
    .aggregate(macros, dbrary, media, logbackAccess)
    .dependsOn(macros, dbrary, media, logbackAccess)
    .settings(
      libraryDependencies ++= appDependencies,
      play.twirl.sbt.Import.TwirlKeys.templateImports ++= Seq("macros._", "site._"),
      PlayKeys.closureCompilerOptions += "ecmascript5_strict",
      PlayKeys.javascriptEntryPoints := PathFinder.empty, // disable play's standard js compiler
      resourceGenerators in Compile := Seq(),
      resourceGenerators in Compile <+= (resourceManaged in Compile, version) map { (dir, ver) =>
	val f = dir / "properties"
	val content = "name=" + appName + "\nversion=" + ver + "\n"
	if (!f.exists || !IO.read(f).equals(content))
	  IO.write(f, content)
	Seq(f)
      },
      resourceGenerators in Compile <+= play.Play.LessCompiler,
      resourceGenerators in Compile <+= AngularTemplate.Compiler,
      resourceGenerators in Compile <+= JSConcatCompiler.Compiler
    )
}
