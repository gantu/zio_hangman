lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "kg.baaber",
      scalaVersion := "2.12.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "hangman",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.0-RC9"
    ),
    trapExit := false
  )
