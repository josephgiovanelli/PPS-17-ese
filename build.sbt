name := "PPS-17-ese"

version := "0.1.0"

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

import Tests._

{
  def groupByFirst(tests: Seq[TestDefinition]) =
    tests map(t => Group(t., Seq(t), SubProcess(ForkOptions())))

  testGrouping in Test := groupByFirst( (definedTests in Test).value )
}