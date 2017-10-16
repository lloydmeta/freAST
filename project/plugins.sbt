lazy val neoScalafmtVersion = "1.12"
addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % neoScalafmtVersion)
addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % neoScalafmtVersion)

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC12")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")