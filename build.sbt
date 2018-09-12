name := "PPS-17-ese"

version := "0.2.0"

scalaVersion := "2.12.6"

//fatJAR creation config
Compile/mainClass := Some("it.unibo.pps.ese.Launcher")
mainClass in assembly := Some("it.unibo.pps.ese.Launcher")
target in assembly := file("target/libs/")
assemblyJarName in assembly := name.value + "_fatjar.jar"
test in assembly := {}

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

Test / fork := true

//markup-loading dependencies
libraryDependencies += "org.yaml" % "snakeyaml" % "1.21"
libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.0"
libraryDependencies += "org.danilopianini" % "thread-inheritable-resource-loader" % "0.3.0"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.13"
)
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"
libraryDependencies += "it.unibo.alice.tuprolog" % "tuprolog" % "3.2"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.0"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.0"
//libraryDependencies += "de.jensd" % "fontawesomefx-commons" % "9.1.2"
//libraryDependencies += "de.jensd" % "fontawesomefx-fontawesome" % "4.7.0-9.1.2"
//libraryDependencies += "de.jensd" % "fontawesomefx-commons" % "8.15"

// https://mvnrepository.com/artifact/de.jensd/fontawesomefx
libraryDependencies += "de.jensd" % "fontawesomefx-commons" % "8.15"
libraryDependencies += "de.jensd" % "fontawesomefx-emojione" % "2.2.7-2"
libraryDependencies += "de.jensd" % "fontawesomefx-materialdesignfont" % "1.7.22-4"
libraryDependencies += "de.jensd" % "fontawesomefx-fontawesome" % "4.7.0-5"
libraryDependencies += "de.jensd" % "fontawesomefx-controls" % "8.15"
libraryDependencies += "de.jensd" % "fontawesomefx-icons525" % "3.0.0-4"
libraryDependencies += "org.controlsfx" % "controlsfx" % "8.40.11"
libraryDependencies += "de.jensd" % "fontawesomefx-weathericons" % "2.0.10-5"

resolvers ++= Seq("Spring Milestone Repository" at "http://repo.spring.io/milestone",
  "Spring Snapshot Repository" at "http://repo.spring.io/snapshot",
  "Spring Release Repository" at "http://repo.spring.io/release",
   "Spring plugins Repository" at "http://repo.spring.io/plugins-release/")

import Tests._
import CustomSettings._

{
  def isolateIncompatibleTests(tests: Seq[TestDefinition]) =
    tests.groupBy(t => {
      if(aloneTests.contains(t.name))
        t.name
      else
        "DefaultGroup"
    }) map(t => Group(t._1, t._2, SubProcess(ForkOptions()))) toSeq

  testGrouping in Test := isolateIncompatibleTests( (definedTests in Test).value )
}