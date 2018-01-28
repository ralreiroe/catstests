
name := "catstests"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.spire-math" %% "spire" % "0.13.0",
  "org.typelevel" % "cats_2.12" % "0.9.0",
  "joda-time" % "joda-time" % "2.9.9",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.typesafe" % "config" % "1.3.2"

)
