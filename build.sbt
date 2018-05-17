// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
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
