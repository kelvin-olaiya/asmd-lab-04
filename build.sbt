import java.io.File
import Dependecies.*

ThisBuild / scalaVersion := "3.7.2"
ThisBuild / organization := "io.github.kelvindev15"

enablePlugins(CucumberPlugin)
CucumberPlugin.glues := List("example.scenarios")

scalafmtDetailedError := true
/*
 * Wartremover
 */
addCompilerPlugin(
  "org.wartremover" %% "wartremover" % "3.4.0" cross CrossVersion.full
)
// see: https://www.wartremover.org/doc/warts.html
scalacOptions ++= Seq("Null").map(wart =>
  s"-P:wartremover:traverser:org.wartremover.warts.${wart}"
)

Compile / doc / target := baseDirectory.value / "api"

lazy val asmd_lab_04 = (project in file("."))
  .settings(
    name := "Asmd Lab 04",
    scalaVersion := "3.7.2",
    libraryDependencies ++= scalaTestBundle,
    libraryDependencies ++= cucumberBundle,
    libraryDependencies += scalaTestJUnit5,
    libraryDependencies ++= mockitoBundle,
    libraryDependencies += scalaCheck
  )
