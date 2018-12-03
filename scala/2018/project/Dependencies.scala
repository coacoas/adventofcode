import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.5"
  lazy val refined = "eu.timepit" %% "refined" % "0.9.3"
  lazy val refinedScalaCheck = "eu.timepit" %% "refined-scalacheck_1.13" % "0.9.3"
}
