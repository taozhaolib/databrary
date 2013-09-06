scalaVersion in ThisBuild := "2.10.2"

scalacOptions in ThisBuild ++= Seq("-feature","-Xlint")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

play.Project.templatesImport += "util._"
