               name :=  "scalaParser"
       scalaVersion :=  "2.11.4"
     testFrameworks +=  new TestFramework("utest.runner.JvmFramework")
libraryDependencies ++= Seq(
  "org.improving" %% "psp-std"   % "0.5.0",
  "com.lihaoyi"   %% "utest"     % "0.2.4",
  "org.parboiled" %% "parboiled" % "2.0.1"
)
