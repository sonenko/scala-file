import sbt._

object Dependencies {

  object log {
    val logback = "ch.qos.logback" % "logback-classic" % "1.1.2"
    val scalaloggingSlf4j = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"

    private val slf4jVersion = "1.7.7"
    val jclOverSlf4j = "org.slf4j" % "jcl-over-slf4j" % slf4jVersion
    val julToSlf4j = "org.slf4j" % "jul-to-slf4j" % slf4jVersion
    val log4jOverSlf4j = "org.slf4j" % "log4j-over-slf4j" % slf4jVersion
    val slf4jApi = "org.slf4j" % "slf4j-api" % slf4jVersion
  }

  object tests {
    def lib(name: String) = "org.specs2" %% s"specs2-$name" % "2.4.17" % "test"
    val mockito = lib("mock")
    val specs2 = lib("core")
  }

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"
  val shapeless = "com.chuusai" %%  "shapeless" % "2.1.0"
}