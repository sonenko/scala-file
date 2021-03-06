import sbt._, Keys._
import sbtrelease.{Version, ReleasePlugin}
import sbtrelease.ReleasePlugin._

import Dependencies._

object Build extends Build {

  val publishSettings = Seq(
    publishMavenStyle := true,
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in (Compile, packageSrc) := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  ) ++ releaseSettings

  val commonSettings = Seq(
    organization := "com.github.sonenko",
    description := "",
    updateOptions := updateOptions.value withCachedResolution true,
    scalaVersion := "2.11.6",
    cancelable in Global := true,
    scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature", "-Xlog-reflective-calls", "-Xfuture", "-Xlint"),
    testOptions in Test := Seq(Tests.Filter(x => x.endsWith("Test"))),
    parallelExecution in Test := false
  )

  lazy val sfile = (project in file("."))
    .settings(libraryDependencies ++= Seq(
      log.logback, log.scalaloggingSlf4j, log.jclOverSlf4j, log.julToSlf4j, log.log4jOverSlf4j, log.slf4jApi,
      tests.specs2, tests.mockito,
      scalaz, shapeless
    ))
    .settings(commonSettings:_*)
}