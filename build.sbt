import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "aesa",
      scalaVersion := "2.12.6",
      version := "0.0.1"
    )),
  name := "mapOfMathematics",
  libraryDependencies ++= Seq(
    scalaTest               % Test,
    "net.ruippeixotog"      %% "scala-scraper" % "2.1.0",
    "com.softwaremill.sttp" %% "core" % "1.2.3",
    "com.softwaremill.sttp" %% "async-http-client-backend-cats" % "1.2.3",
    "io.circe"              %% "circe-core" % "0.9.3",
    "io.circe"              %% "circe-generic" % "0.9.3",
    "org.typelevel"         %% "cats-effect" % "1.0.0-RC2",
    "org.typelevel"         %% "cats-core" % "1.1.0",
    "com.lihaoyi"           %% "pprint" % "0.5.3",
    "com.lihaoyi"           %% "fastparse" % "0.4.2",
    "com.github.pathikrit"  %% "better-files" % "3.5.0",
    "com.github.tomtung"    %% "latex2unicode" % "0.2.4"
  )
)
