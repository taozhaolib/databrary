scalaVersion in ThisBuild := "2.10.2"

scalacOptions in ThisBuild ++= Seq("-feature","-Xlint")

// scalacOptions += "-Ymacro-debug-lite"

resolvers in ThisBuild += Resolver.file("Local repo", file(Path.userHome.absolutePath+"/.ivy2/local"))(Resolver.ivyStylePatterns)

libraryDependencies in ThisBuild += "postgresql" % "postgresql" % "9.1-901-1.jdbc4" // "9.2-1002.jdbc4"

libraryDependencies in ThisBuild += "fi.reaktor" %% "sqltyped" % "0.3.0-SNAPSHOT"

initialize ~= { _ =>
  System.setProperty("sqltyped.driver", "org.postgresql.Driver")
  System.setProperty("sqltyped.url", "jdbc:postgresql:databrary?datatype.inet=dbrary.PGinet&datatype.permission=dbrary.PGpermission&datatype.audit_action=dbrary.PGaudit_action")
  System.setProperty("sqltyped.schema", "public")
  System.setProperty("sqltyped.username", "databrary")
  System.setProperty("sqltyped.password", "")
}
