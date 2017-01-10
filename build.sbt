name := "Binarized Neural Net"
scalaVersion := "2.11.8"

lazy val root = Project("bnn", file("."))
  .dependsOn(RootProject(file("./fpga-tidbits/")))
  .settings(
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release",
    scalacOptions ++= Seq("-deprecation",
                          "-feature",
                          "-unchecked",
                          "-language:reflectiveCalls")
  )
