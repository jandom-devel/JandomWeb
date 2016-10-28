name := """JandomWeb"""

version := "0.5-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "it.unich.jandom" %% "jandom" % "0.1.3-SNAPSHOT",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

fork in run := true

// Add PPL jar when found

val optionalPPLPathName = try {
    val PPLPathName = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }

unmanagedJars in Compile ++= (optionalPPLPathName map file).toSeq
