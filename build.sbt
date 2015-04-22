name := "expressions-scala"

version := "0.1"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.0.1",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test
)

ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := """.*\.Calculator"""
