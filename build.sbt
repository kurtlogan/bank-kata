import Dependencies._

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "bank-kata",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.12.0"
  )

