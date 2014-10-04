// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.5")


addSbtPlugin("org.databrary" % "sbt-angular-templates" % "0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-stylus" % "1.0.1")


addSbtPlugin("com.typesafe.sbt" % "sbt-coffeescript" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-uglify" % "1.0.3")


// resolvers += Resolver.url("bintray-eamelink-sbt-plugins", url("http://dl.bintray.com/eamelink/sbt-plugins"))(Resolver.ivyStylePatterns)

// addSbtPlugin("net.eamelink.sbt" % "sbt-purescript" % "0.5.0-SNAPSHOT")

val sbtPurescript = uri("git://github.com/eamelink/sbt-purescript")


lazy val root = (project in file("."))
  .dependsOn(sbtPurescript)
