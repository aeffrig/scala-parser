def common = Seq(
         scalaVersion :=  "2.11.4",
  libraryDependencies ++= Seq(
    "org.improving" %% "psp-std"   % "0.5.0",
    "org.parboiled" %% "parboiled" % "2.0.2-SNAPSHOT"
  )
)

val macros = project settings (common: _*) settings (
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
)

val parser = project dependsOn macros settings (common: _*) settings (
  name := "scalaParser",
  test := (run in Test toTask "").value
)
