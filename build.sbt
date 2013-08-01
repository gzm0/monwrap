scalaVersion := "2.10.1"

scalacOptions += "-deprecation"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
