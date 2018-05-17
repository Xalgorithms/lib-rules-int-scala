lazy val VERSION_SCALA               = "2.11.11"
lazy val VERSION_MONGO_SCALA         = "2.3.0"
lazy val VERSION_PLAY_JSON           = "2.6.0"
lazy val VERSION_BETTER_FILES        = "3.4.0"
//lazy val VERSION_JACKSON_DATABIND    = "2.6.5"
lazy val VERSION_SCALA_TEST          = "3.0.5"
lazy val VERSION_SCALA_MOCK          = "4.1.0"

lazy val settings = Seq(
  name := "xa-rules-interpreter",
  version := "0.0.1",
  organization := "http://xalgorithms.org",
  scalaVersion := VERSION_SCALA
)

// lazy val depOverrides = Seq(
//   "com.fasterxml.jackson.core" % "jackson-databind"       % VERSION_JACKSON_DATABIND
// )

lazy val deps = Seq(
  "com.typesafe.play"      %% "play-json"                 % VERSION_PLAY_JSON,
  // for org.bson
  "org.mongodb.scala"      %% "mongo-scala-driver"        % VERSION_MONGO_SCALA,
  "org.scalactic"          %% "scalactic"                 % VERSION_SCALA_TEST,
  "com.github.pathikrit"   %% "better-files"              % VERSION_BETTER_FILES,
  "org.scalatest"          %% "scalatest"                 % VERSION_SCALA_TEST % "test",
  "org.scalamock"          %% "scalamock"                 % VERSION_SCALA_MOCK % "test"
)

lazy val root = (project in file("."))
  .settings(settings)
//  .settings(dependencyOverrides ++= depOverrides)
  .settings(libraryDependencies ++= deps)
