val dottyVersion = "0.11.0-bin-20181101-714ce80-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "hello",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
