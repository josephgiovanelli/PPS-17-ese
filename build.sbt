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

libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.0"

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