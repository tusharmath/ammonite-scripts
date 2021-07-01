lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalaVersion := "2.13.6",
    fork:= true,
    libraryDependencies :=
      Seq(
        "dev.zio" %% "zio"         % "1.0.8",
        "dev.zio" %% "zio-streams" % "1.0.8",
      ),
  )
