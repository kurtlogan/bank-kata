import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT",
      scalafmtOnCompile := true
    )
  ),
  name := "bank-kata",
  libraryDependencies += parserCombinators,
  libraryDependencies += scalaTest % Test,
  libraryDependencies += scalaCheck % Test
)
