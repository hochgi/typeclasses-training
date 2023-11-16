import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ai.hunters"
ThisBuild / organizationName := "hunters"

lazy val root = (project in file("."))
  .settings(
    name := "typeclasses-training",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.6",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest" %% "scalatest" % "3.2.17" % Test)
  )
