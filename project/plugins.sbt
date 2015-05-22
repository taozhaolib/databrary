// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.9")


addSbtPlugin("org.databrary" % "sbt-angular-templates" % "0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-stylus" % "1.0.1")


addSbtPlugin("com.typesafe.sbt" % "sbt-coffeescript" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-uglify" % "1.0.3")

lazy val root = (project in file("."))
