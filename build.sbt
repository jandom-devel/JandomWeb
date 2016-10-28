name := """JandomWeb"""

version := "0.5-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  cache,
  jdbc,
  ws,
  specs2 % Test,
  "it.unich.jandom" %% "jandom" % "0.1.3-SNAPSHOT"
)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

fork in run := true

// Add PPL jar when found

val optionalPPLPathName = try {
    val PPLPathName = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }

unmanagedJars in Compile ++= (optionalPPLPathName map file).toSeq
