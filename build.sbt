scalacOptions in ThisBuild ++= Seq("-feature","-Xlint")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

scalacOptions in (Compile, doc) <++= baseDirectory.map { bd => Seq(
  "-sourcepath", bd.getAbsolutePath,
  "-doc-source-url", "https://github.com/databrary/play/tree/master€{FILE_PATH}.scala"
) }

play.Project.templatesImport += "util._"

GitDescribe.gitDescribeOptions in ThisBuild := Seq("--tags", "--dirty")

version in ThisBuild <<= GitDescribe.gitDescribe.apply(_.getOrElse("unknown"))
