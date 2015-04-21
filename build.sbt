name := "databrary"

scalaVersion in ThisBuild := "2.11.6"

scalacOptions in ThisBuild ++= Seq("-target:jvm-1.7","-optimise","-feature","-deprecation","-Xlint","-Yinline-warnings")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

scalacOptions in (Compile, doc) <++= baseDirectory.map { bd => Seq(
  "-sourcepath", bd.getAbsolutePath,
  "-doc-source-url", "https://github.com/databrary/databrary/tree/master€{FILE_PATH}.scala"
) }

GitDescribe.gitDescribeOptions in ThisBuild := Seq("--long", "--dirty") // "--first-parent"

version in ThisBuild <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown"))

lazy val macros = project

lazy val dbrary = project
  .dependsOn(macros)

lazy val media = project
  .dependsOn(dbrary)

lazy val databrary = (project in file("."))
  .enablePlugins(PlayScala, SbtWeb)
  .dependsOn(macros, dbrary, media)

libraryDependencies ++= Seq(
  "org.databrary" %% "play-logback-access" % "0.2",
  "org.databrary" %% "iteratees" % "0.1",
  "org.mindrot" % "jbcrypt" % "0.3m",
  ws,
  "com.typesafe.play" %% "play-mailer" % "2.4.0",
  "com.github.fge" % "json-schema-validator" % "2.2.6",
  "org.webjars" % "jquery" % "1.11.2",
  "org.webjars" % "angularjs" % "1.3.15",
  "org.webjars" % "ng-flow" % "2.6.1",
  "org.webjars" % "normalize.styl" % "3.0.0",
  "org.webjars" % "lodash" % "3.6.0"
)

mappings in Universal ~= { s =>
  s.filterNot(_._2.startsWith("share/doc/api/"))
}

com.typesafe.sbt.packager.universal.Keys.dist <<= com.typesafe.sbt.packager.universal.Keys.packageZipTarball in Universal

resourceGenerators in Compile <+= (resourceManaged in Compile, name, version) map { (dir, name, ver) =>
  val f = dir / "properties"
  val p = new java.util.Properties()
  p.setProperty("name", name)
  p.setProperty("version", ver)
  IO.write(p, "build info", f)
  Seq(f)
}

TwirlKeys.templateImports ++= Seq("macros._", "site._")

JsEngineKeys.engineType := JsEngineKeys.EngineType.Node

pipelineStages in Assets := Seq(uglify)


StylusKeys.useNib in Assets := true

includeFilter in (Assets, StylusKeys.stylus) := "app.styl"

StylusKeys.compress in (Assets, StylusKeys.stylus) := true

JsTaskKeys.jsOptions in (Assets, StylusKeys.stylus) ~= { (s : String) =>
  import spray.json._
  JsObject(s.parseJson.asJsObject.fields
    /* unfortunate hard-coding: */
    .+("paths" -> JsArray(JsString("target/web/web-modules/main/webjars/lib/normalize.styl")))
  ).toString
}


AngularTemplatesKeys.module := "app"

AngularTemplatesKeys.compressRemoveIntertagSpaces := true

AngularTemplatesKeys.outputHtml := None


CoffeeScriptKeys.bare := true

JsTaskKeys.sourceDependencies in JshintKeys.jshint += CoffeeScriptKeys.coffeescript

WebKeys.jsFilter in Assets := new SimpleFileFilter({ f =>
  f.getName.endsWith(".js") &&
    (f.getPath.startsWith((sourceDirectory in Assets).value.getPath) ||
     f.getPath.startsWith((resourceManaged in (Assets, CoffeeScriptKeys.coffeescript)).value.getPath))
})

includeFilter in (Assets, uglify) := (WebKeys.jsFilter in Assets).value

UglifyKeys.uglifyOps := { js =>
  // we assume that app.js is first alphabetically
  Seq((js.sortBy(_._2), "app.min.js"))
}

UglifyKeys.define := Some("DEBUG=false")
