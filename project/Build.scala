import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "databrary"

  val dbDependencies = Seq(
    jdbc,
    "com.github.mauricio" %% "postgresql-async" % "0.2.12"
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
      "org.postgresql" % "postgresql" % "9.3-1100-jdbc4"
    )
  )

  val media = Project("media", file("media")).dependsOn(dbrary).settings(
    libraryDependencies ++= avDependencies
  )

  val logbackAccess = Project("logback-access", file("logback-access"))

  val appDependencies = dbDependencies ++ avDependencies ++ Seq(
    "org.mindrot" % "jbcrypt" % "0.3m",
    "com.typesafe" %% "play-plugins-mailer" % "2.2.0"
  )

  val jsExterns = Seq(
    url("http://code.jquery.com/jquery-1.11.0.min.js"),
    url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular.min.js"),
    url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-route.min.js"),
    url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-sanitize.min.js"),
    url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-resource.min.js"),
    url("https://github.com/Pasvaz/bindonce/raw/0.3.1/bindonce.min.js"),
    url("https://github.com/gsklee/ngStorage/raw/0.3.0/ngStorage.min.js")
  )

  val main = play.Project(appName, "unknown", appDependencies)
    .dependsOn(macros, dbrary, media, logbackAccess).settings(
      version <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown")),
      play.Project.templatesImport ++= Seq("macros._", "site._"),
      javascriptEntryPoints := PathFinder.empty, // disable play's standard js compiler
      resourceGenerators in Compile := Seq(),
      resourceGenerators in Compile <+= (resourceManaged in Compile, version) map { (dir, ver) =>
	val f = dir / "properties"
	val content = "name=" + appName + "\nversion=" + ver + "\n"
	if (!f.exists || !IO.read(f).equals(content))
	  IO.write(f, content)
	Seq(f)
      },
      resourceGenerators in Compile <+= play.Project.LessCompiler,
      resourceGenerators in Compile <+= AngularTemplate.Compiler,
      JSConcatCompiler.externs := jsExterns,
      resourceGenerators in Compile <+= JSConcatCompiler.Compiler
    )
}
