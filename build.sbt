import psp.libsbt.Deps

val real = taskKey[Unit]("parse real sources discoverable through real/ subdirectory")

def scratchSources = fromBase("scratch") map (_ * "*.scala" get)

def common = standardSettings ++ Seq(
        scalacOptions +=  "-Yno-predef",
            resolvers +=  Opts.resolver.sonatypeSnapshots,
            resolvers +=  "paulp/maven" at "https://dl.bintray.com/paulp/maven",
         scalaVersion :=  "2.11.4",
  libraryDependencies ++= Seq(
    "org.improving" %% "psp-pio"   %     "0.5.3",
    "org.parboiled" %% "parboiled" % "2.1.0-SNAPSHOT",
    "org.improving" %% "expecty"   %    "1.0.0-RC4"    % "test"
  )
)

lazy val macros: Project = project also common also (libraryDependencies += Deps.scalaReflect.value)

lazy val parser: Project = project dependsOn macros also common also (
                 name :=  "psp-parser",
  libraryDependencies +=  Deps.scalaCompiler.value % "test",
         key.initRepl <+= resourceDirectory in Compile mapValue (d => IO.read(d / "initialCommands.scala"))
)

lazy val root = project.root aggregate (macros, parser) settings (
        real  :=  (runMain in parser in Test toTask " psp.parser.tests.RealSourcesTest real").value,
        test  :=  (runMain in parser in Test toTask " psp.parser.tests.UnitTests").value,
         run <<=  run in Compile in parser,
watchSources <++= scratchSources,
     console <<=  console in parser in Compile
)
