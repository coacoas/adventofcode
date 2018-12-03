import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "2018",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.3.1",
      refined % Test,
      refinedScalaCheck % Test,
      scalaTest % Test,
      scalaCheck % Test
    )
  )
