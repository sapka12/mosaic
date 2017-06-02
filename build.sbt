name := "mosaic"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.16"

libraryDependencies += "com.sksamuel.scrimage" % "scrimage-core_2.12" % "2.1.8"
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-filters_2.12" % "2.1.8"
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-io-extra_2.12" % "2.1.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
