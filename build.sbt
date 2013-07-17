scalaVersion := "2.10.1"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
