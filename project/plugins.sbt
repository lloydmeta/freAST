lazy val neoScalafmtVersion = "1.12"
addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % neoScalafmtVersion)
addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % neoScalafmtVersion)

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC12")