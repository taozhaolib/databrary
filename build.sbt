scalaVersion := "2.10.4"

scalacOptions in ThisBuild ++= Seq("-target:jvm-1.7","-optimise","-feature","-deprecation","-Xlint","-Yinline-warnings")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

scalacOptions in (Compile, doc) <++= baseDirectory.map { bd => Seq(
  "-sourcepath", bd.getAbsolutePath,
  "-doc-source-url", "https://github.com/databrary/databrary/tree/master€{FILE_PATH}.scala"
) }

GitDescribe.gitDescribeOptions in ThisBuild := Seq("--long", "--dirty")

version in ThisBuild <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown"))

JSConcatCompiler.externs := Seq(
  url("http://code.jquery.com/jquery-1.11.0.min.js"),
  url("https://github.com/23/resumable.js/raw/master/resumable.js"),
  url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular.min.js"),
  url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-route.min.js"),
  url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-sanitize.min.js"),
  url("https://ajax.googleapis.com/ajax/libs/angularjs/1.2.15/angular-resource.min.js"),
  url("https://github.com/Pasvaz/bindonce/raw/0.3.1/bindonce.min.js"),
  url("https://github.com/gsklee/ngStorage/raw/0.3.0/ngStorage.min.js")
)
