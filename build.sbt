               name :=  "scalaParser"
       scalaVersion :=  "2.11.4"
               test :=  (run in Test toTask "").value
libraryDependencies ++= Seq(
  "org.improving" %% "psp-std"   % "0.5.0",
  "org.parboiled" %% "parboiled" % "2.0.2-SNAPSHOT"
)
