libraryDependencies ++= Seq(
  component("play"),
  jdbc,
  "com.github.mauricio" %% "postgresql-async" % "0.2.15",
  "org.postgresql" % "postgresql" % "9.3-1102-jdbc41"
)
