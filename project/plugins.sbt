// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-uglify" % "1.0.3")

lazy val angularTemplates = project in file("angular-templates")

libraryDependencies += "com.googlecode.htmlcompressor" % "htmlcompressor" % "1.5.2"

lazy val root = (project in file(".")).dependsOn(angularTemplates)
