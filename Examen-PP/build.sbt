ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

lazy val root = (project in file("."))
  .settings(
    name := "Examen-PP"
  )
