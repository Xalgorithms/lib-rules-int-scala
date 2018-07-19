// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This file is part of Interlibr, a functional component of an
// Internet of Rules (IoR).

// ACKNOWLEDGEMENTS
// Funds: Xalgorithms Foundation
// Collaborators: Don Kelly, Joseph Potvin and Bill Olders.

// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
lazy val VERSION_SCALA               = "2.12.4"
lazy val VERSION_MONGO_SCALA         = "2.3.0"
lazy val VERSION_PLAY_JSON           = "2.6.0"
lazy val VERSION_BETTER_FILES        = "3.4.0"
lazy val VERSION_SCALA_TEST          = "3.0.5"
lazy val VERSION_SCALA_MOCK          = "4.1.0"
lazy val VERSION_FAKER               = "0.15"

lazy val settings = Seq(
  name := "il-rules-interpreter",
  version := "0.0.5",
  organization := "org.xalgorithms",
  scalaVersion := VERSION_SCALA
)

lazy val deps = Seq(
  "com.typesafe.play"      %% "play-json"                 % VERSION_PLAY_JSON,
  "com.github.javafaker"   %  "javafaker"                 % VERSION_FAKER,
  // for org.bson
  "org.mongodb.scala"      %% "mongo-scala-driver"        % VERSION_MONGO_SCALA,
  "org.scalactic"          %% "scalactic"                 % VERSION_SCALA_TEST,
  "com.github.pathikrit"   %% "better-files"              % VERSION_BETTER_FILES,
  "org.scalatest"          %% "scalatest"                 % VERSION_SCALA_TEST % "test",
  "org.scalamock"          %% "scalamock"                 % VERSION_SCALA_MOCK % "test"
)

lazy val root = (project in file("."))
  .settings(settings)
  .settings(libraryDependencies ++= deps)
