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
      "org.typelevel" %% "cats-core" % "1.5.0",
      "org.typelevel" %% "cats-effect" % "1.1.0",
      "co.fs2" %% "fs2-core" % "1.0.2",
      "co.fs2" %% "fs2-io" % "1.0.2",
      refined % Test,
      refinedScalaCheck % Test,
      scalaTest % Test,
      scalaCheck % Test
    )
  )
