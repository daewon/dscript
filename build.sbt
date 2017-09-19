name := "sisp"

version := "0.1"

sbtVersion := "1.0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Yrangepos"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "com.lihaoyi" %% "fastparse" % "0.4.4",
  "jline" % "jline" % "2.12"
)

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots"
)


initialCommands += """
"""

initialCommands in console += """
import scala.language._
import scala.language.implicitConversions._
"""
