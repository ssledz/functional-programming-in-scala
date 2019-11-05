name := "scala-fp"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)