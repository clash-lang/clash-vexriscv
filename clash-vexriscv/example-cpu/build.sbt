// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

import Dependencies._

val spinalVersion = "1.10.1"

ThisBuild / scalaVersion     := "2.11.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.google"
ThisBuild / organizationName := "Google"

lazy val root = (project in file("."))
  .settings(
    name := "example-cpu",
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
      "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion),
      "org.yaml" % "snakeyaml" % "1.8"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
