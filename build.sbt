scalaVersion := "2.10.4"

scalacOptions in ThisBuild ++= Seq("-target:jvm-1.7","-optimise","-feature","-deprecation","-Xlint","-Yinline-warnings")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

scalacOptions in (Compile, doc) <++= baseDirectory.map { bd => Seq(
  "-sourcepath", bd.getAbsolutePath,
  "-doc-source-url", "https://github.com/databrary/databrary/tree/master€{FILE_PATH}.scala"
) }

GitDescribe.gitDescribeOptions in ThisBuild := Seq("--tags", "--dirty")

version in ThisBuild <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown"))
