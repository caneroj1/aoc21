import Dependencies._

ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "1.0.0"
ThisBuild / organization     := "com.aoc"
ThisBuild / organizationName := "aoc"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2021",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
