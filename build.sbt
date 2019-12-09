name := "sk.scala.algorithms"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.10.3",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
