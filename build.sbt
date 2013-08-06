name := "ChefInterpreter"

version := "1.0"

scalaVersion := "2.9.1"

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12.4" % "test"
)
