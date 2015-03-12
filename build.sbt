name := "coffee-break"

scalaVersion := "2.11.6"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "junit" % "junit" % "4.4" % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.4",
  "com.typesafe.akka" %% "akka-agent" % "2.3.4",
  "c3p0" % "c3p0" % "0.9.1.2",
  "mysql" % "mysql-connector-java" % "5.1.31")

